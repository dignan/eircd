-module(eircd_channel_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name) ->
    supervisor:start_child(?MODULE, [Name]).

init([]) ->
    Children = [
        {
            eircd_channel,
            {eircd_channel, start_link, []},
            temporary, 5000, worker, [eircd_channel]
        }
    ],
    {ok, { {simple_one_for_one, 0, 1}, Children} }.
