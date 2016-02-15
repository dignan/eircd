-module(eircd_rate_limiter).
-behaviour(gen_server).

-export([start_link/2]).
-export([send_event/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          max_events,
          period,
          events = queue:new()
         }).

start_link(MaxEvents, Period) ->
    gen_server:start_link(?MODULE, [MaxEvents, Period], []).

send_event(Pid) ->
    gen_server:cast(Pid, {event, eircd_utils:timestamp()}).

init([MaxEvents, Period]) ->
    {ok, #state{max_events = MaxEvents, period = Period}}.

handle_call({event, Timestamp}, _From, State=#state{events=Events,
                                                    period=Period,
                                                    max_events=MaxEvents}) ->
    NewEvents = remove_old_events(queue:in(Timestamp, Events),
                                  eircd_utils:timestamp() - Period),
    IsRateLimited = queue:len(NewEvents) > MaxEvents,
    {reply, IsRateLimited, State#state{events = Events}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

remove_old_events(Events, CurrentTimeMinusPeriod) ->
    case queue:out(Events) of
        {{value, E}, Events2} when E < CurrentTimeMinusPeriod ->
            remove_old_events(Events2, CurrentTimeMinusPeriod);
        {_, Events2} ->
            Events2
    end.
