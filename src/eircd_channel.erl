-module(eircd_channel).
-behaviour(gen_server).
-export([start_link/1]).
-export([join/3, part/6, send_message/3, nick/3, topic/6, member_count/1, topic/1, name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([gproc_key/1]).

-record(state, {
    name,
    topic = <<>>,
    members = [],
    names = []
}).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

join(ChannelPid, Pid, Nick) ->
    gen_server:call(ChannelPid, {join, Pid, Nick}).

part(ChannelPid, Pid, Nick, User, Address, PartMessage) ->
    gen_server:call(ChannelPid, {part, Pid, Nick, User, Address, PartMessage}).

nick(Pid, OldNick, Nick) ->
    gen_server:call(Pid, {nick, OldNick, Nick}).

send_message(ChannelPid, Pid, Message) ->
    gen_server:call(ChannelPid, {send, Pid, Message}).

topic(ChannelPid, Pid, Nick, User, Address, Topic) ->
    gen_server:call(ChannelPid, {topic, Pid, Nick, User, Address, Topic}).

topic(ChannelPid) ->
    gen_server:call(ChannelPid, topic).

member_count(ChannelPid) ->
    gen_server:call(ChannelPid, member_count).

name(ChannelPid) ->
    gen_server:call(ChannelPid, name).

init([Name]) ->
    true = gproc:reg({n, l, gproc_key(Name)}),
    true = gproc:reg({p, l, {module, ?MODULE}}),
    {ok, #state{name = Name}}.

handle_call({join, Pid, Nick}, _From, State=#state{members=Members, topic=Topic, names=Names}) ->
    case lists:member(Pid, Members) of
        true -> {reply, {error, alreadyjoined}, State};
        false -> {reply, {ok, Topic}, State#state{members = [Pid|Members], names = [Nick|Names]}}
    end;
handle_call({part, Pid, Nick, User, Address, PartMessage}, From, State=#state{members=Members, names=Names, name=Name}) ->
    case lists:member(Pid, Members) of
        true ->
            Message = eircd_irc_messages:part(
                Nick,
                User,
                Address,
                Name,
                PartMessage),
            handle_call({send, Pid, Message}, From, State),
            Members2 = lists:delete(Pid, Members),
            if length(Members2) =< 0 -> {stop, normal, State};
                true -> {reply, ok, State#state{members = lists:delete(Pid, Members), names = lists:delete(Nick, Names)}}
            end;
        false ->
            {reply, {error, notonchannel}, State}
    end;
handle_call({topic, Pid, Nick, User, Address, Topic}, _From, State=#state{members=Members, name=Name}) ->
    case lists:member(Pid, Members) of
        true ->
            Message = eircd_irc_messages:topic(
                Nick,
                User,
                Address,
                Name,
                Topic),
            [eircd_irc_protocol_fsm:send_message(M, Message) || M <- Members],
            {reply, ok, State#state{topic = Topic}};
        false ->
            {reply, {error, notonchannel}, State}
    end;
handle_call({send, Pid, Message}, _From, State=#state{members=Members}) ->
    Reply = case lists:member(Pid, Members) of
        true ->
            [eircd_irc_protocol_fsm:send_message(M, Message) || M <- lists:delete(Pid, Members)],
            ok;
        false ->
            {error, cannotsendtochan}
    end,
    {reply, Reply, State};
handle_call(member_count, _From, State=#state{members=Members}) ->
    {reply, {ok, length(Members)}, State};
handle_call(topic, _From, State=#state{topic=Topic}) ->
    {reply, {ok, Topic}, State};
handle_call(name, _From, State=#state{name=Name}) ->
    {reply, {ok, Name}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, undefined}, State}.

handle_cast({nick, OldNick, Nick}, State=#state{names = Names}) ->
    {noreply, State#state{names = [Nick|lists:delete(OldNick, Names)]}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

gproc_key(Name) -> {channel, Name}.


