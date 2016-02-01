-module(eircd_server).
-include("eircd.hrl").
-behaviour(gen_server).
-export([start_link/0]).
-export([channel/1, nick/1, nick/2, list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    nicks = gb_sets:new(),
    channels = gb_sets:new(),
    channel_refs = dict:new()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

channel(Channel) ->
    gen_server:call(?MODULE, {channel, Channel}).

nick(Nick) ->
    gen_server:call(?MODULE, {nick, Nick}).

nick(OldNick, Nick) ->
    gen_server:call(?MODULE, {nick, OldNick, Nick}).

list() ->
    gen_server:call(?MODULE, list).

init([]) ->
    {ok, #state{}}.

handle_call({channel, Channel}, _From, State=#state{channels=Channels, channel_refs=ChannelRefs}) ->
    case validate_name(Channel) of
        false ->
            {reply, {error, nosuchchannel}, State};
        true -> 
            case gb_sets:is_subset(gb_sets:singleton(Channel), Channels) of
                true ->
                    {reply, {ok, gproc:lookup_pid({n, l, eircd_channel:gproc_key(Channel)})}, State};
                false ->
                    {ok, Pid} = eircd_channel_sup:start_child(Channel),
                    Ref = erlang:monitor(process, Pid),
                    {reply, {ok, Pid}, State#state{channels = gb_sets:add(Channel, Channels),
                                                   channel_refs = dict:append(Ref, Channel, ChannelRefs)}}
            end
    end;
handle_call({nick, Nick}, _From, State=#state{nicks=Nicks}) ->
    case gb_sets:is_subset(gb_sets:singleton(Nick), Nicks) of
        true -> {reply, {error, nicknameinuse}, State};
        false -> {reply, ok, State#state{nicks = gb_sets:add(Nick, Nicks)}}
    end;
handle_call({nick, OldNick, Nick}, From, State=#state{nicks=Nicks}) ->
    Nicks2 = gb_sets:subtract(Nicks, gb_sets:singleton(OldNick)),
    handle_call({nick, Nick}, From, State#state{nicks=Nicks2});
handle_call(list, _From, State=#state{channels = Channels}) ->
    {reply, lists:map(fun make_channel_list_reply/1, gb_sets:to_list(Channels)), State};
handle_call(_Request, _From, State) ->
    {reply, {error, undefined}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid2, _Reason}, State=#state{channels=Channels, channel_refs=ChannelRefs}) ->
    lager:info("Dead channel"),
    {ok, Channel} = dict:find(Ref, ChannelRefs),
    {noreply, State#state{
                channels = gb_sets:subtract(Channels, gb_sets:singleton(Channel)),
                channel_refs = dict:erase(Ref, ChannelRefs)
    }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

make_channel_list_reply(Channel) ->
    Pid = gproc:lookup_pid({n, l, eircd_channel:gproc_key(Channel)}),
    {ok, Name} = eircd_channel:name(Pid),
    {ok, MemberCount} = eircd_channel:member_count(Pid),
    {ok, Topic} = eircd_channel:topic(Pid),
    {Name, MemberCount, Topic}.

validate_name(Name) when length(Name) > 50 -> false;
validate_name(Name) -> 
    case binary:first(Name) of
        $# -> true;
        _ -> false
    end.
