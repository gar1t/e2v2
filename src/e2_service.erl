-module(e2_service).

-behaviour(gen_server).

-export([start_link/2, start_link/3, call/2, call/3, cast/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mod,
                cb_state,
                init,
                handle_info,
                terminate,
                timeout_cb}).

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
    dispatch_init(init_state(Module), Args).

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

gen_server_options(_Options) -> [].

init_state(Module) when is_atom(Module) ->
    #state{mod=Module,
           init=e2_util:optional_function(Module, init, 1),
           handle_info=e2_util:optional_function(Module, handle_info, 2),
           terminate=e2_util:optional_function(Module, terminate, 2)}.

dispatch_init(#state{init=undefined}=State, Args) ->
    handle_init_result({ok, Args}, State);
dispatch_init(#state{init=Init}=State, Args) ->
    handle_init_result(e2_util:apply_handler(Init, [Args]), State).

handle_init_result({ok, CbState}, State) ->
    {ok, set_cb_state(State, CbState)};
handle_init_result({ok, CbState, {_Function, _Args}=Callback}, State) ->
    {ok, set_timeout_cb(set_cb_state(State, CbState), Callback), 0};
handle_init_result({ok, CbState, Timeout}, State)
  when is_integer(Timeout) ->
    {ok, set_cb_state(State, CbState), Timeout};
handle_init_result({stop, Reason}, _State) ->
    {stop, Reason};
handle_init_result(ignore, _State) ->
    ignore;
handle_init_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_call({Function, Args}, From,
              #state{mod=Module, cb_state=CbState}=State)
              when is_atom(Function) ->
    handle_call_result(
      e2_util:apply_handler({Module, Function, Args}, [From, CbState]),
      State).

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

dispatch_cast({Function, Args},
              #state{mod=Module, cb_state=CbState}=State)
              when is_atom(Function) ->
    handle_cast_result(
      e2_util:apply_handler({Module, Function, Args}, [CbState]),
      State).

handle_cast_result({noreply, CbState}, State) ->
    {noreply, set_cb_state(State, CbState)};
handle_cast_result({stop, Reason, CbState}, State) ->
    {stop, Reason, set_cb_state(State, CbState)};
handle_cast_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_info(timeout, #state{mod=Module, cb_state=CbState,
                              timeout_cb={Function, Args}}=State) ->
    handle_info_result(
      e2_util:apply_handler({Module, Function, Args}, [CbState]),
      clear_timeout_cb(State));
dispatch_info(_Msg, #state{handle_info=undefined}=State) ->
    {noreply, State};
dispatch_info(Msg, #state{handle_info=Handler, cb_state=CbState}=State) ->
    handle_info_result(e2_util:apply_handler(Handler, [Msg, CbState]), State).

handle_info_result({noreply, CbState}, State) ->
    {noreply, set_cb_state(State, CbState)};
handle_info_result({noreply, CbState, Timeout}, State) ->
    {noreply, set_cb_state(State, CbState), Timeout};
handle_info_result({stop, Reason, CbState}, State) ->
    {stop, Reason, set_cb_state(State, CbState)};
handle_info_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_terminate(_Reason, #state{terminate=undefined}) ->
    ok;
dispatch_terminate(Reason, #state{terminate=Handler, cb_state=CbState}) ->
    e2_util:apply_handler(Handler, [Reason, CbState]).

set_cb_state(State, CbState) ->
    State#state{cb_state=CbState}.

set_timeout_cb(State, Callback) ->
    State#state{timeout_cb=Callback}.

clear_timeout_cb(State) ->
    State#state{timeout_cb=undefined}.
