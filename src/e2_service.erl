-module(e2_service).

-behaviour(gen_server).

-export([start_link/2, start_link/3, call/2, call/3, cast/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{handle_msg, 3}].

-record(state, {mod, mod_state, timeout_msg}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Module, Args) ->
    start_link(Module, Args, []).

%%--------------------------------------------------------------------
%% @doc Starts a service.
%% @spec start_link(Module, Options) -> {ok, Pid} | {error, Reason}
%% Module = atom()
%% Options = [option()]
%% option() = registered | {registered, Name}
%% Name = atom()
%% @end
%%--------------------------------------------------------------------

start_link(Module, Args, Options) ->
    start_gen_server(server_name(Module, Options), Module, Args, Options).

call(ServiceRef, Handler) ->
    call(ServiceRef, Handler, infinity).

call(ServiceRef, Handler, Timeout) ->
    gen_server:call(ServiceRef, {'$call', Handler}, Timeout).

cast(ServiceRef, Handler) ->
    gen_server:cast(ServiceRef, {'$cast', Handler}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Module, Args}) ->
    State = init_state(Module),
    maybe_trap_exit(State),
    dispatch_init(Module, Args, State).

handle_call({'$call', Handler}, From, State) ->
    dispatch_call(Handler, From, State).

handle_cast({'$cast', Handler}, State) ->
    dispatch_cast(Handler, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    dispatch_info(Msg, State).

terminate(Reason, State) ->
    dispatch_terminate(Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

server_name(Module, Options) ->
    case proplists:get_value(registered, Options) of
        undefined -> unregistered;
        true -> {registered, Module};
        Name when is_atom(Name) -> {registered, Name};
        Other -> error({badarg, Other})
    end.

start_gen_server(unregistered, Module, Args, Options) ->
    gen_server:start_link(
      ?MODULE, {Module, Args}, gen_server_options(Options));
start_gen_server({registered, Name}, Module, Args, Options) ->
    gen_server:start_link(
      {local, Name}, ?MODULE, {Module, Args}, gen_server_options(Options)).

%% TODO: What do we want to pass through to gen_server?
gen_server_options(_Options) -> [].

init_state(Module) when is_atom(Module) ->
    #state{mod=Module}.

maybe_trap_exit(#state{mod=Mod}) ->
    case e2_util:optional_function(Mod, terminate, 2) of
        undefined -> ok;
        _ -> process_flag(trap_exit, true)
    end.

dispatch_init(Module, Args, State) ->
    case e2_util:optional_function(Module, init, 1) of
        undefined ->
            handle_init_result({ok, Args}, State);
        Init ->
            handle_init_result(e2_util:apply_handler(Init, [Args]), State)
    end.

handle_init_result({ok, ModState}, State) ->
    {ok, set_mod_state(ModState, State)};
handle_init_result({ok, ModState, FirstMsg}, State) ->
    {ok, set_timeout_msg(FirstMsg, set_mod_state(ModState, State)), 0};
handle_init_result({stop, Reason}, _State) ->
    {stop, Reason};
handle_init_result(ignore, _State) ->
    ignore;
handle_init_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_call(Msg, From, #state{mod=Module, mod_state=ModState}=State) ->
    handle_call_result(dispatch_msg(Module, Msg, From, ModState), State).

handle_call_result({reply, Reply, ModState}, State) ->
    {reply, Reply, set_mod_state(ModState, State)};
handle_call_result({noreply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_call_result({noreply, ModState, NextMsg}, State) ->
    {noreply, set_timeout_msg(NextMsg, set_mod_state(ModState, State)), 0};
handle_call_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_call_result({stop, Reason, Reply, ModState}, State) ->
    {stop, Reason, Reply, set_mod_state(ModState, State)};
handle_call_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_cast(Msg, #state{mod=Module, mod_state=ModState}=State) ->
    handle_cast_result(dispatch_msg_noreply(Module, Msg, ModState), State).

handle_cast_result({noreply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_cast_result({noreply, ModState, NextMsg}, State) ->
    {noreply, set_timeout_msg(NextMsg, set_mod_state(ModState, State)), 0};
handle_cast_result({reply, _Reply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_cast_result({reply, _Reply, ModState, NextMsg}, State) ->
    {noreply, set_timeout_msg(NextMsg, set_mod_state(ModState, State)), 0};
handle_cast_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_cast_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_info(timeout, #state{mod=Module, mod_state=ModState,
                              timeout_msg=Msg}=State) when Msg =/= undefined ->
    handle_info_result(
      dispatch_msg_noreply(Module, Msg, ModState), clear_timeout_msg(State));
dispatch_info(Msg, #state{mod=Module, mod_state=ModState}=State) ->
    handle_info_result(dispatch_msg_noreply(Module, Msg, ModState), State).

handle_info_result({noreply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_info_result({noreply, ModState, NextMsg}, State) ->
    {noreply, set_timeout_msg(NextMsg, set_mod_state(ModState, State)), 0};
handle_info_result({reply, _Reply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_info_result({reply, _Reply, ModState, NextMsg}, State) ->
    {noreply, set_timeout_msg(NextMsg, set_mod_state(ModState, State)), 0};
handle_info_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_info_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_terminate(Reason, #state{mod=Mod, mod_state=ModState}) ->
    case e2_util:optional_function(Mod, terminate, 2) of
        undefined -> ok;
        Terminate ->
            e2_util:apply_handler(Terminate, [Reason, ModState])
    end.

dispatch_msg_noreply(Module, Msg, ModState) ->
    Module:handle_msg(Msg, noreply, ModState).

dispatch_msg(Module, Msg, From, ModState) ->
    Module:handle_msg(Msg, From, ModState).

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.

set_timeout_msg(Msg, State) ->
    State#state{timeout_msg=Msg}.

clear_timeout_msg(State) ->
    State#state{timeout_msg=undefined}.
