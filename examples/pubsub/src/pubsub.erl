-module(pubsub).

-behavior(e2_application).

-export([start/0]).

-export([init/0]).

start() ->
    application:start(sasl),
    application:start(pubsub).

init() ->
    {ok, [broker]}.
