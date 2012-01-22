-module(repeating).

-behavior(e2_task).

-export([start_link/2, handle_task/1]).

start_link(Delay, Repeat) ->
    e2_task:start_link(?MODULE, 1, [{delay, Delay}, {repeat, Repeat}]).

handle_task(N) ->
    io:format(user, "~f - exec ~b~n", [timestamp(), N]),
    {continue, N + 1}.

timestamp() ->
    {M, S, U} = erlang:now(),
    M * 1000000 + S + U / 1000000.
