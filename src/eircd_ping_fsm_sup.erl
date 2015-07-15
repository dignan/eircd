-module(eircd_ping_fsm_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Protocol) ->
  supervisor:start_child(?MODULE, [Protocol]).

init([]) ->
  Children = [
    {
      eircd_ping_fsm,
      {eircd_ping_fsm, start_link, []},
      temporary, 5000, worker, [eircd_ping_fsm]
    }
  ],
  {ok, { {simple_one_for_one, 5, 10}, Children} }.
