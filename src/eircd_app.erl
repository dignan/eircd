-module(eircd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(ranch),
    application:start(gproc),
    {ok, _} = ranch:start_listener(irc, 100,
        ranch_tcp, [{port, 6667}],
        eircd_irc_protocol, []
    ),
    eircd_sup:start_link().

stop(_State) ->
    ok.
