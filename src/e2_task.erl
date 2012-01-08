-module(e2_task).

-behavior(e2_service).

-export([start_link/2, start_link/3]).

-export([init/1, handle_msg/3, terminate/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{handle_task, 1}].

-record(state, {mod, mod_state, handle_task}).

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
    handle_init_result(dispatch_init(Module, Args), Module).

handle_msg('$handle_task', noreply, State) ->
    dispatch_handle_task(State).

terminate(Reason, State) ->
    dispatch_terminate(Reason, State).

%%%===================================================================
%%% Internal functionsa
%%%===================================================================

dispatch_init(Module, Args) ->
    case e2_util:optional_function(Module, init, 1) of
        undefined -> {ok, Args};
        Init -> Init(Args)
    end.

handle_init_result({ok, ModState}, Module) ->
    {ok, init_state(Module, ModState), '$handle_task'};
handle_init_result({ok, ModState, DelayTask}, Module) ->
    erlang:send_after(DelayTask, self(), '$handle_task'),
    {ok, init_state(Module, ModState)};
handle_init_result({stop, Reason}, _) ->
    {stop, Reason};
handle_init_result(ignore, _) ->
    ignore;
handle_init_result(Other, _) ->
    error({bad_return_value, Other}).

init_state(Module, ModState) ->
    #state{mod=Module,
           mod_state=ModState,
           handle_task=e2_util:required_function(Module, handle_task, 1)}.

dispatch_handle_task(#state{handle_task=Handle, mod_state=ModState}=State) ->
    handle_task_result(Handle(ModState), State).

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

dispatch_terminate(Reason, #state{mod=Mod, mod_state=ModState}) ->
    case e2_util:optional_function(Mod, terminate, 2) of
        undefined -> ok;
        Terminate ->
            e2_util:apply_handler(Terminate, [Reason, ModState])
    end.

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.
