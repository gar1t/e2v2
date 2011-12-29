-module(sequence).

-behavior(e2_service).

start_link() ->
    e2_service:start_link(?MODULE, [registered, {state, 1}]).

next() ->
    e2_service:call(?MODULE, {handle_next, []}).

reset() -> reset(1).

reset(Start) ->
    e2_service:cast(?MODULE, {handle_reset, [Start]}).

handle_next(_From, Next) ->
    {reply, Next, Next + 1}.

handle_reset(NewNext, _OldNext) ->
    {noreply, NewNext}.
