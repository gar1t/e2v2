-module(e2_task).

-behavior(e2_service).

-export([start_link/2, start_link/3,
         call/2, call/3, cast/2]).

-export([init/1, handle_msg/3, terminate/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{handle_task, 1}].

-record(state, {mod, mod_state, start, repeat}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Module, Args) ->
    start_link(Module, Args, []).

start_link(Module, Args, Options) ->
    {ServiceOpts, TaskOpts} = e2_service_impl:split_options(Module, Options),
    e2_service:start_link(?MODULE, {Module, Args, TaskOpts}, ServiceOpts).

call(Task, Msg) ->
    e2_service:call(Task, Msg).

call(Task, Msg, Timeout) ->
    e2_service:call(Task, Msg, Timeout).

cast(Task, Msg) ->
    e2_service:cast(Task, Msg).

%%%===================================================================
%%% e2_service callbacks
%%%===================================================================

init({Module, Args, TaskOpts}) ->
    e2_service_impl:set_trap_exit(Module),
    dispatch_init(Module, Args, TaskOpts, init_state(Module)).

handle_msg('$handle_task', noreply, State) ->
    dispatch_handle_task(set_start(State));
handle_msg(Msg, From, #state{mod=Module, mod_state=ModState0}=State) ->
    {Result, ModState} =
        e2_service_impl:dispatch_handle_msg(Module, Msg, From, ModState0),
    e2_service_impl:handle_msg_result(Result, set_mod_state(ModState, State)).

terminate(Reason, State) ->
    dispatch_terminate(Reason, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state(Module) ->
    #state{mod=Module}.

dispatch_init(Module, Args, TaskOpts, State) ->
    case erlang:function_exported(Module, init, 1) of
        true ->
            handle_init_result(Module:init(Args), State);
        false ->
            handle_init_result({ok, Args, timing_spec(TaskOpts)}, State)
    end.

timing_spec(Options) ->
    case {proplists:get_value(delay, Options),
          proplists:get_value(repeat, Options)}
    of
        {undefined, undefined} -> 0;
        {undefined, Repeat} -> {0, Repeat};
        {Delay, undefined} -> Delay;
        {Delay, Repeat} -> {Delay, Repeat}
    end.

handle_init_result({ok, ModState}, State) ->
    {ok, set_mod_state(ModState, State), '$handle_task'};
handle_init_result({ok, ModState, {0, Repeat}}, State) ->
    {ok, set_repeat(Repeat, set_mod_state(ModState, State)), '$handle_task'};
handle_init_result({ok, ModState, {Delay, Repeat}}, State) ->
    erlang:send_after(Delay, self(), '$handle_task'),
    {ok, set_repeat(Repeat, set_mod_state(ModState, State))};
handle_init_result({ok, ModState, 0}, State) ->
    {ok, set_mod_state(ModState, State), '$handle_task'};
handle_init_result({ok, ModState, Delay}, Module) ->
    erlang:send_after(Delay, self(), '$handle_task'),
    {ok, set_mod_state(ModState, Module)};
handle_init_result({stop, Reason}, _) ->
    {stop, Reason};
handle_init_result(ignore, _) ->
    ignore;
handle_init_result(Other, _) ->
    error({bad_return_value, Other}).

dispatch_handle_task(#state{mod=Module, mod_state=ModState}=State) ->
    handle_task_result(Module:handle_task(ModState), State).

handle_task_result({continue, ModState}, State) ->
    case repeat_delay(State) of
        0 ->
            {noreply, set_mod_state(ModState, State), '$handle_task'};
        Delay ->
            erlang:send_after(Delay, self(), '$handle_task'),
            {noreply, set_mod_state(ModState, State)}
    end;
handle_task_result({continue, ModState, Delay}, State) ->
    erlang:send_after(Delay, self(), '$handle_task'),
    {noreply, set_mod_state(ModState, State)};
handle_task_result({stop, Reason}, State) ->
    {stop, Reason, State};
handle_task_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_task_result({hibernate, ModState}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_task_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_terminate(Reason, #state{mod=Module, mod_state=ModState}) ->
    e2_service_impl:dispatch_terminate(Module, Reason, ModState).

repeat_delay(#state{repeat=undefined}) -> 0;
repeat_delay(#state{repeat=Interval, start=Start}) ->
    Now = timestamp(),
    ((Now - Start) div Interval + 1) * Interval + Start - Now.

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.

set_repeat(Repeat, State) ->
    State#state{repeat=Repeat}.

set_start(#state{start=undefined}=State) ->
    State#state{start=timestamp()};
set_start(State) -> State.

timestamp() ->
    {M, S, U} = erlang:now(),
    M * 1000000000 + S * 1000 + U div 1000.
