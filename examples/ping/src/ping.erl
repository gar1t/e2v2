-module(ping).

-behavior(e2_application).

-export([start/0, ping/0]).

-export([init/0]).

start() ->
    application:start(sasl),
    application:start(ping).

ping() ->
    ping_server:ping().

init() ->
    {ok, [ping_server]}.
