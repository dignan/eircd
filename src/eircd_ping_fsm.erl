-module(eircd_ping_fsm).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([mark_activity/1, pong/2]).
-export([connecting/2, active/2, waiting/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {idle_timeout, response_timeout, protocol, token}).

start_link(Protocol) ->
    gen_fsm:start_link(?MODULE, [Protocol], []).

mark_activity(Pid) ->
    gen_fsm:send_event(Pid, activity).

pong(Pid, Token) ->
    gen_fsm:send_event(Pid, {pong, Token}).

init([Protocol]) ->
    true = gproc:reg({p, l, {module, ?MODULE}}),
    {ok, IdleTimeout} = application:get_env(eircd, ping_idle_timeout),
    {ok, ResponseTimeout} = application:get_env(eircd, ping_response_timeout),
    {ok, connecting, #state{
        idle_timeout = IdleTimeout,
        response_timeout = ResponseTimeout,
        protocol = Protocol
    }}.

connecting(activity, State) -> {next_state, active, State, State#state.idle_timeout}.

active(activity, State) -> {next_state, active, State, State#state.idle_timeout};
active(timeout, State) -> ping(State).

ping(State) ->
    Token = eircd_utils:random_string(8),
    eircd_irc_protocol:send_message(State#state.protocol, eircd_irc_messages:ping(Token)),
    {next_state, waiting, State#state{token = Token}, State#state.response_timeout}.

waiting({pong, Token}, State=#state{token=Token}) -> {next_state, active, State};
waiting({pong, _}, State) -> {next_state, waiting, State};
waiting(timeout, _State) -> exit(timeout).

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
