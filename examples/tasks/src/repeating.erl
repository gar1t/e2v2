-module(repeating).

-behavior(e2_task).

-export([start_link/2, reset/1, handle_task/1, handle_msg/3]).

start_link(Delay, Repeat) ->
    e2_task:start_link(?MODULE, 1, [{delay, Delay}, {repeat, Repeat}]).

reset(Task) ->
    e2_task:cast(Task, reset).

handle_task(N) ->
    io:format(user, "~f - exec ~b~n", [timestamp(), N]),
    {continue, N + 1}.

handle_msg(reset, _From, _N) ->
    {reply, ok, 1}.

timestamp() ->
    {M, S, U} = erlang:now(),
    M * 1000000 + S + U / 1000000.
