-module(eircd_irc_protocol_fsm).
-behaviour(gen_fsm).
-include("eircd.hrl").

-export([start_link/2]).
-export([send_message/2]).
-export([pass/2, nick_and_user/2, connected/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
          protocol,
          nick,
          user,
          realname,
          address,
          servername,
          ping_fsm,
          channels = [],
          pass = undefined,
          pass_provided = undefined,
          user_modes = [],
          channel_modes = []
}).

start_link(Protocol, Address) ->
    gen_fsm:start_link(?MODULE, [Protocol, Address], []).

send_message(Pid, Message) ->
    gen_fsm:send_event(Pid, {send, Message}).

init([Protocol, Address]) ->
    true = gproc:reg({p, l, {module, ?MODULE}}),
    {ok, ServerName} = application:get_env(eircd, servername),
    {ok, ConnectTimeout} = application:get_env(eircd, connect_timeout),
    {ok, PingFsm} = eircd_ping_fsm_sup:start_child(Protocol),
    Pass = application:get_env(eircd, pass, undefined),
    link(PingFsm),
    {ok, pass, #state{
        address = Address,
        protocol = Protocol,
        servername = ServerName,
        ping_fsm = PingFsm,
        pass = Pass
    }, ConnectTimeout}.

pass({irc, {_, <<"PASS">>, [Pass], _}}, State) ->
    {next_state, nick_and_user, State#state{pass_provided = binary_to_list(Pass)}};
pass(M, State) -> nick_and_user(M, State).

%% Ignore CAP so we don't break v3 clients
nick_and_user({irc, {_, <<"CAP">>, _, _}}, State) ->
    {next_state, nick_and_user, State};
nick_and_user({irc, {_, <<"NICK">>, [Nick], _}}, State=#state{nick=undefined}) ->
    case eircd_server:nick(Nick) of
        {error, nicknameinuse} ->
            Reply = eircd_irc_messages:err_nicknameinuse(State#state.servername, Nick),
            eircd_irc_protocol:send_message(State#state.protocol, Reply),
            {next_state, nick_and_user, State};
        ok ->
            lager:info("Registering ~p", [Nick]),
            true = gproc:reg({n, l, gproc_key(Nick)}),
            maybe_welcome(State#state{nick = Nick})
    end;
nick_and_user({irc, {_, <<"USER">>, [User, _Hostname, _Servername], RealName}}, State=#state{user=undefined}) ->
    true = gproc:reg({p, l, gproc_user_property(User)}),
    maybe_welcome(State#state{user = User, realname = RealName});
nick_and_user(timeout, _State) ->
    exit(timeout).

maybe_welcome(State=#state{pass=Pass, pass_provided=Pass2}) when Pass =/= Pass2,
								 Pass =/= undefined,
								 pass_provided =/= undefined ->
    eircd_irc_protocol:send_message(State#state.protocol, {<<"ERROR">>, [], <<"Closing Link (Password mismatch)">>}),
    exit(password_mismatch);
maybe_welcome(State=#state{nick=Nick, user=User}) when Nick =/= undefined, User =/= undefined ->
    welcome(State);
maybe_welcome(State) ->
    {next_state, nick_and_user, State}.

welcome(State) ->
    eircd_irc_protocol:send_message(
      State#state.protocol,
      eircd_irc_messages:rpl_welcome(
	      State#state.servername,
	      State#state.nick)),
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    ok = motd(State),
    {next_state, connected, State}.

connected({irc, {_, <<"PING">>, [Token], _}}, State) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    eircd_irc_protocol:send_message(State#state.protocol, eircd_irc_messages:pong(Token)),
    {next_state, connected, State};
connected({irc, {_, <<"NICK">>, [NewNick], _}}, State=#state{nick=Nick, user=User, address=Address}) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    case eircd_server:nick(Nick, NewNick) of
        {error, nicknameinuse} ->
            Reply = eircd_irc_messages:err_nicknameinuse(State#state.servername, Nick),
            eircd_irc_protocol:send_message(State#state.protocol, Reply),
            {next_state, connected, State};
        ok ->
            lager:info("Unregistering ~p", [Nick]),
            lager:info("Registering ~p", [NewNick]),
            true = gproc:unreg({n, l, gproc_key(Nick)}),
            true = gproc:reg({n, l, gproc_key(NewNick)}),
            Message = eircd_irc_messages:nick(Nick, User, Address, NewNick),
            eircd_irc_protocol:send_message(State#state.protocol, Message),
            send_message_to_channels(State#state.channels, Message),
            eircd_channel:nick(self(), Nick, NewNick),
            {next_state, connected, State#state{nick=NewNick}}
    end;
connected({irc, {_, <<"PRIVMSG">>, [Targets], MessageText}}, State) when is_list(Targets) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    State2 = lists:foldl(
               fun(Target, S) ->
                       privmsg(Target, MessageText, S)
               end,
               State,
               Targets),
    {next_state, connected, State2};
connected({irc, {_, <<"PRIVMSG">>, [Target], MessageText}}, State) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    {next_state, connected, privmsg(Target, MessageText, State)};
connected({irc, {_, <<"JOIN">>, [Channels|_], _}}, State) when is_list(Channels) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    State2 = lists:foldl(
               fun(Channel, S) ->
                       join(Channel, S)
               end,
               State,
               Channels),
    {next_state, connected, State2};
connected({irc, {_, <<"JOIN">>, [Channel|_], _}}, State)->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    {next_state, connected, join(Channel, State)};
connected({irc, {_, <<"PART">>, [Channels], PartMessage}}, State) when is_list(Channels) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    State2 = lists:foldl(
               fun(Channel, S) ->
                       part(Channel, PartMessage, S)
               end,
               State,
               Channels),
    {next_state, connected, State2};
connected({irc, {_, <<"PART">>, [Channel], PartMessage}}, State) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    {next_state, connected, part(Channel, PartMessage, State)};
connected({irc, {_, <<"TOPIC">>, [Channel], undefined}}, State) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    case gproc:where({n, l, eircd_channel:gproc_key(Channel)}) of
        undefined ->
	    eircd_irc_protocol:send_message(
	      State#state.protocol,
	      eircd_irc_messages:err_nosuchchannel(
	        State#state.servername,
                Channel)),
	    {next_state, connected, State};
        Pid ->
            {ok, Topic} = eircd_channel:topic(Pid),
            send_topic_or_notopic(
              State#state.protocol,
              State#state.servername,
              State#state.nick,
              Channel,
              Topic),
            {next_state, connected, State}
    end;
connected({irc, {_, <<"TOPIC">>, [Channel], Topic}}, State) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    case gproc:where({n, l, eircd_channel:gproc_key(Channel)}) of
        undefined ->
	    eircd_irc_protocol:send_message(
	      State#state.protocol,
	      eircd_irc_messages:err_nosuchchannel(
	        State#state.servername,
                Channel)),
	    {next_state, connected, State};
        Pid ->
	    R = eircd_channel:topic(
		  Pid,
                  self(),
		  State#state.nick,
		  State#state.user,
		  State#state.address,
		  Topic),
	    case R of
	        {error, nosuchchannel} ->
		    eircd_irc_protocol:send_message(
		      State#state.protocol,
                      eircd_irc_messages:err_nosuchchannel(
		        State#state.servername,
		        Channel)),
		    {next_state, connected, State};
	        ok ->
		    {next_state, connected, State}
             end
    end;
connected({irc, {_, <<"MOTD">>, _, _}}, State) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    motd(State),
    {next_state, connected, State};
connected({irc, {_, <<"LIST">>, _, _}}, State) ->
    eircd_ping_fsm:mark_activity(State#state.ping_fsm),
    eircd_irc_protocol:send_message(
      State#state.protocol,
      eircd_irc_messages:rpl_liststart(
        State#state.servername,
        State#state.nick)),
    Messages = lists:map(make_list_reply(State), eircd_server:list()),
    lists:foreach(
      fun(M) -> 
              eircd_irc_protocol:send_message(State#state.protocol, M)
      end,
      Messages),
    eircd_irc_protocol:send_message(
      State#state.protocol,
      eircd_irc_messages:rpl_listend(
        State#state.servername,
        State#state.nick)),
    {next_state, connected, State};
connected({send, Message}, State) ->
    eircd_irc_protocol:send_message(State#state.protocol, Message),
    {next_state, connected, State};
connected(Event, State) ->
    lager:info("Unmatched event: ~p", [Event]),
    {next_state, connected, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, undefined}, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

join(Channel, State) ->
    case eircd_server:channel(Channel) of
        {error, nosuchchannel} ->
            lager:info("Invalid channel name ~p", [Channel]),
            eircd_irc_protocol:send_message(
              State#state.protocol,
              eircd_irc_messages:err_nosuchchannel(State#state.servername, Channel)),
            State;
        {ok, Pid} ->
            join(Channel, Pid, State)
    end.

join(Channel, Pid, State=#state{channels=Channels}) ->
    case eircd_channel:join(Pid, self(), State#state.nick) of
        {error, alreadyjoined} ->
            State;
        {ok, Topic} ->
            Message = eircd_irc_messages:join(
			State#state.nick,
			State#state.user,
			State#state.address,
			Channel),
            eircd_irc_protocol:send_message(State#state.protocol, Message),
            eircd_channel:send_message(Pid, self(), Message),
            maybe_send_topic(
              State#state.protocol,
              State#state.servername,
              State#state.nick,
              Channel,
              Topic),
            lager:info("Joined channel: ~p <- ~p", [Channel, State#state.nick]),
            State#state{channels = [Channel|Channels]}
    end.

privmsg(Target, MessageText, State) ->
    case get_target_pid(Target) of
        undefined ->
            eircd_irc_protocol:send_message(
	      State#state.protocol,
	      eircd_irc_messages:err_nosuchnick(
		State#state.servername,
		State#state.nick,
		Target)),
            State;
        TargetPid ->
            send_message_to_nick_or_channel(
	      TargetPid,
	      eircd_irc_messages:privmsg(
		State#state.nick,
                State#state.user,
                State#state.address,
                Target,
                MessageText)),
            State
    end.

part(Channel, undefined, State=#state{nick=Nick}) -> part(Channel, Nick, State);
part(Channel, PartMessage, State=#state{channels=Channels}) ->
    case gproc:where({n, l, eircd_channel:gproc_key(Channel)}) of
        undefined ->
            eircd_irc_protocol:send_message(
	      State#state.protocol,
	      eircd_irc_messages:err_nosuchnick(State#state.servername, State#state.nick, Channel)),
            {next_state, connected, State};
        Pid ->
            case eircd_channel:part(Pid, self(), State#state.nick, State#state.user, State#state.address, PartMessage) of
                {error, notonchannel} ->
                    eircd_irc_protocol:send_message(
		      State#state.protocol,
		      eircd_irc_messages:err_notonchannel(
                        State#state.servername,
                        State#state.nick,
                        Channel)),
                    {next_state, connected, State};
                ok ->
                    eircd_irc_protocol:send_message(
		      State#state.protocol,
		      eircd_irc_messages:part(
                        State#state.nick,
                        State#state.user,
                        State#state.address,
                        Channel,
                        PartMessage)),
                    {next_state, connected, State#state{channels = lists:delete(Channel, Channels)}}
            end
    end.

make_list_reply(#state{servername=ServerName, nick=Nick}) ->
    fun({ChannelName, MemberCount, Topic}) ->
            eircd_irc_messages:rpl_list(
              ServerName,
              Nick,
              ChannelName,
              MemberCount,
              Topic)
    end.

gproc_key(Nick) -> {nick, Nick}.

gproc_user_property(User) -> {user, User}.

%% FIXME: handle {error, cannotsendtochan}
send_message_to_channels(Channels, Message) ->
    [eircd_channel:send_message(gproc:lookup_pid({n, l, eircd_channel:gproc_key(C)}), self(), Message) || C <- Channels].

target_type(Target) ->
    case binary:first(Target) of
        $# -> channel;
        _ -> nick
    end.

get_target_pid(Target) ->
    case target_type(Target) of
        channel ->
            case gproc:where({n, l, eircd_channel:gproc_key(Target)}) of
                undefined -> undefined;
                Pid -> {channel, Pid}
            end;
        nick ->
            case gproc:where({n, l, gproc_key(Target)}) of
                undefined -> undefined;
                Pid -> {nick, Pid}
            end
    end.

send_message_to_nick_or_channel({nick, Pid}, Message) ->
    eircd_irc_protocol_fsm:send_message(Pid, Message);
send_message_to_nick_or_channel({channel, Pid}, Message) ->
    eircd_channel:send_message(Pid, self(), Message).



send_topic_or_notopic(Protocol, ServerName, Nick, Channel, <<>>) ->
    eircd_irc_protocol:send_message(
      Protocol,
      eircd_irc_messages:rpl_notopic(ServerName, Nick, Channel));
send_topic_or_notopic(Protocol, Servername, Nick, Channel, Topic) ->
    eircd_irc_protocol:send_message(
      Protocol,
      eircd_irc_messages:rpl_topic(Servername, Nick, Channel, Topic)).

maybe_send_topic(_, _, _, _, <<>>) ->
    ok;
maybe_send_topic(Protocol, Servername, Nick, Channel, Topic) ->
    eircd_irc_protocol:send_message(
      Protocol,
      eircd_irc_messages:rpl_topic(Servername, Nick, Channel, Topic)).

motd(State) ->
    case get_motd() of
	{error, nomotd} ->
	    no_motd(State);
	Content ->
	    motd(Content, State)
    end.

motd(Content, State) ->
    eircd_irc_protocol:send_message(
      State#state.protocol,
      eircd_irc_messages:rpl_motdstart(State#state.servername, State#state.nick)),
    Motd = lists:flatmap(
	     fun(S) ->
		     binary:split(S, <<"\r">>, [global])
	     end,
	     binary:split(Content, <<"\n">>, [global])),
    lists:foreach(
      fun(Line) ->
	      eircd_irc_protocol:send_message(
		State#state.protocol,
		eircd_irc_messages:rpl_motd(
		  State#state.servername,
		  State#state.nick,
		  Line))
      end,
      Motd),
    eircd_irc_protocol:send_message(
      State#state.protocol,
      eircd_irc_messages:rpl_motdend(State#state.servername, State#state.nick)),
    ok.

no_motd(State) ->
    eircd_irc_protocol:send_message(
      State#state.protocol,
      eircd_irc_messages:rpl_nomotd(
	State#state.servername,
	State#state.nick)).

get_motd() ->    
    get_motd(application:get_env(eircd, motdfile)).

get_motd(undefined) ->
    get_motd("/etc/eircd/motd.txt");
get_motd({ok, File}) ->
    get_motd(File);
get_motd(File) ->
    case file:read_file(File) of
	{ok, Contents} ->
	    Contents;
	{error, enoent} ->
	    {error, nomotd}
    end.
