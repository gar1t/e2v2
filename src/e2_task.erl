-module(e2_task).

-behavior(e2_service).

-export([start_link/2, start_link/3]).

-export([init/1, handle_msg/3, terminate/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{handle_task, 1}].

-record(state, {mod, mod_state}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Module, Args) ->
    e2_service:start_link(?MODULE, {Module, Args}).

start_link(Module, Args, Options) ->
    e2_service:start_link(?MODULE, {Module, Args}, Options).

%%%===================================================================
%%% e2_service callbacks
%%%===================================================================

init({Module, Args}) ->
    dispatch_init(Module, Args, init_state(Module)).

handle_msg('$handle_task', noreply, State) ->
    dispatch_handle_task(State).

terminate(Reason, State) ->
    dispatch_terminate(Reason, State).

%%%===================================================================
%%% Internal functionsa
%%%===================================================================

init_state(Module) ->
    #state{mod=Module}.

dispatch_init(Module, Args, State) ->
    case erlang:function_exported(Module, init, 1) of
        true ->
            handle_init_result(Module:init(Args), State);
        false ->
            handle_init_result({ok, Args}, State)
    end.

handle_init_result({ok, ModState}, State) ->
    {ok, set_mod_state(ModState, State), '$handle_task'};
handle_init_result({ok, ModState, DelayTask}, Module) ->
    erlang:send_after(DelayTask, self(), '$handle_task'),
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
    {noreply, set_mod_state(ModState, State), '$handle_task'};
handle_task_result({continue, ModState, Delay}, State) ->
    erlang:send_after(Delay, self(), '$handle_task'),
    {noreply, set_mod_state(ModState, State)};
handle_task_result({stop, Reason}, State) ->
    {stop, Reason, State};
handle_task_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_task_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_terminate(Reason, #state{mod=Module, mod_state=ModState}) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, ModState);
        false -> ok
    end.

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.
