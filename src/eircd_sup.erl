-module(eircd_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {
            eircd_server,
            {eircd_server, start_link, []},
            permanent, 5000, worker, [eircd_server]
        },
        {
            eircd_channel_sup,
            {eircd_channel_sup, start_link, []},
            permanent, 5000, supervisor, [eircd_channel_sup]
        },
        {
            eircd_ping_fsm_sup,
            {eircd_ping_fsm_sup, start_link, []},
            permanent, 5000, supervisor, [eircd_ping_fsm_sup]
        },
        {
            eircd_irc_protocol_fsm_sup,
            {eircd_irc_protocol_fsm_sup, start_link, []},
            permanent, 5000, supervisor, [eircd_irc_protocol_fsm_sup]
        }
    ],
    {ok, { {one_for_all, 5, 10}, Children} }.
