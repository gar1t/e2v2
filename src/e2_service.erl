-module(e2_service).

-behaviour(gen_server).

-export([start_link/2, start_link/3, call/2, call/3, cast/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{handle_msg, 3}].

-record(state, {mod, cb_state, first_msg}).

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

call(ServerRef, Handler) ->
    call(ServerRef, Handler, infinity).

call(ServerRef, Handler, Timeout) ->
    gen_server:call(ServerRef, {'$call', Handler}, Timeout).

cast(ServerRef, Handler) ->
    gen_server:cast(ServerRef, {'$cast', Handler}).

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
    case proplists:get_value(registered, Options, '$undefined') of
        '$undefined' -> unregistered;
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

handle_init_result({ok, CbState}, State) ->
    {ok, set_cb_state(State, CbState)};
handle_init_result({ok, CbState, Timeout}, State)
  when is_integer(Timeout) ->
    {ok, set_cb_state(State, CbState), Timeout};
handle_init_result({ok, CbState, FirstMsg}, State) ->
    {ok, set_first_msg(set_cb_state(State, CbState), FirstMsg), 0};
handle_init_result({stop, Reason}, _State) ->
    {stop, Reason};
handle_init_result(ignore, _State) ->
    ignore;
handle_init_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_call(Msg, From, #state{mod=Module, cb_state=CbState}=State) ->
    handle_call_result(dispatch_msg(Module, Msg, From, CbState), State).

handle_call_result({reply, Reply, CbState}, State) ->
    {reply, Reply, set_cb_state(State, CbState)};
handle_call_result({reply, Reply, CbState, Timeout}, State) ->
    {reply, Reply, set_cb_state(State, CbState), Timeout};
handle_call_result({noreply, CbState}, State) ->
    {noreply, set_cb_state(State, CbState)};
handle_call_result({noreply, CbState, Timeout}, State) ->
    {noreply, set_cb_state(State, CbState), Timeout};
handle_call_result({stop, Reason, CbState}, State) ->
    {stop, Reason, set_cb_state(State, CbState)};
handle_call_result({stop, Reason, Reply, CbState}, State) ->
    {stop, Reason, Reply, set_cb_state(State, CbState)};
handle_call_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_cast(Msg, #state{mod=Module, cb_state=CbState}=State) ->
    handle_cast_result(dispatch_msg_noreply(Module, Msg, CbState), State).

handle_cast_result({noreply, CbState}, State) ->
    {noreply, set_cb_state(State, CbState)};
handle_cast_result({noreply, CbState, Timeout}, State) ->
    {noreply, set_cb_state(State, CbState), Timeout};
handle_cast_result({reply, _Reply, CbState}, State) ->
    {noreply, set_cb_state(State, CbState)};
handle_cast_result({reply, _Reply, CbState, Timeout}, State) ->
    {noreply, set_cb_state(State, CbState), Timeout};
handle_cast_result({stop, Reason, CbState}, State) ->
    {stop, Reason, set_cb_state(State, CbState)};
handle_cast_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_info(timeout, #state{mod=Module, cb_state=CbState,
                              first_msg=Msg}=State) when Msg =/= undefined ->
    handle_info_result(
      dispatch_msg_noreply(Module, Msg, CbState), clear_first_msg(State));
dispatch_info(Msg, #state{mod=Module, cb_state=CbState}=State) ->
    handle_info_result(dispatch_msg_noreply(Module, Msg, CbState), State).

handle_info_result({noreply, CbState}, State) ->
    {noreply, set_cb_state(State, CbState)};
handle_info_result({noreply, CbState, Timeout}, State) ->
    {noreply, set_cb_state(State, CbState), Timeout};
handle_info_result({reply, _Reply, CbState}, State) ->
    {noreply, set_cb_state(State, CbState)};
handle_info_result({reply, _Reply, CbState, Timeout}, State) ->
    {noreply, set_cb_state(State, CbState), Timeout};
handle_info_result({stop, Reason, CbState}, State) ->
    {stop, Reason, set_cb_state(State, CbState)};
handle_info_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_terminate(Reason, #state{mod=Mod, cb_state=CbState}) ->
    case e2_util:optional_function(Mod, terminate, 2) of
        undefined -> ok;
        Terminate ->
            e2_util:apply_handler(Terminate, [Reason, CbState])
    end.

dispatch_msg_noreply(Module, Msg, CbState) ->
    Module:handle_msg(Msg, noreply, CbState).

dispatch_msg(Module, Msg, From, CbState) ->
    Module:handle_msg(Msg, From, CbState).

set_cb_state(State, CbState) ->
    State#state{cb_state=CbState}.

set_first_msg(State, FirstMsg) ->
    State#state{first_msg=FirstMsg}.

clear_first_msg(State) ->
    State#state{first_msg=undefined}.
