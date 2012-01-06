-module(e2_publisher).

-behavior(e2_service).

-export([start_link/2,
         start_link/3,
         subscribe/2,
         subscribe/3,
         subscribe_all/1,
         subscribe_all/2,
         unsubscribe/2,
         unsubscribe/3,
         unsubscribe_all/1,
         unsubscribe_all/2,
         publish/2]).

-export([init/1, handle_msg/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [].

-record(state, {mod, modstate, handle_msg, subs}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Module, Args) ->
    start_link(Module, Args, []).

start_link(Module, Args, Options) ->
    {ServiceOpts, MyOpts} = e2_service_impl:split_options(Module, Options),
    e2_service:start_link(?MODULE, {Module, Args, MyOpts}, ServiceOpts).

subscribe(Publisher, Pattern) ->
    subscribe(Publisher, Pattern, self()).

subscribe(Publisher, Pattern, Subscriber) ->
    e2_service:call(
      Publisher, {'$sub', Pattern, compile_pattern(Pattern), Subscriber}).

subscribe_all(Publisher) ->
    subscribe_all(Publisher, self()).

subscribe_all(Publisher, Subscriber) ->
    e2_service:call(Publisher, {'$sub_all', Subscriber}).

unsubscribe(Publisher, Pattern) ->
    unsubscribe(Publisher, Pattern, self()).

unsubscribe(Publisher, Pattern, Subscriber) ->
    e2_service:call(Publisher, {'$unsub', Pattern, Subscriber}).

unsubscribe_all(Publisher) ->
    unsubscribe_all(Publisher, self()).

unsubscribe_all(Publisher, Subscriber) ->
    e2_service:call(Publisher, {'$unsub_all', Subscriber}).

publish(Publisher, Msg) ->
    e2_service:cast(Publisher, {'$pub', Msg}).

%%%===================================================================
%%% Service callbacks
%%%===================================================================

init({Module, Args, _Options}) ->
    {Reply, ModState} = e2_service_impl:init(Module, Args),
    e2_service_impl:init_reply(Reply, init_state(Module, ModState)).

handle_msg({'DOWN', _Ref, process, Pid, _Info}, noreply, State) ->
    {noreply, remove_all_subscribers(Pid, State)};
handle_msg({'$sub', Pattern, Compiled, Subscriber}, _From, State0) ->
    {Added, State} = add_subscriber(Pattern, Compiled, Subscriber, State0),
    {reply, Added, State};
handle_msg({'$sub_all', Subscriber}, _From, State0) ->
    {Added, State} = add_subscriber(all, all, Subscriber, State0),
    {reply, Added, State};
handle_msg({'$unsub', Pattern, Subscriber}, _From, State0) ->
    {Removed, State} = remove_subscriber(Pattern, Subscriber, State0),
    {reply, Removed, State};
handle_msg({'$unsub_all', Subscriber}, _From, State0) ->
    {Removed, State} = remove_all_subscribers(Subscriber, State0),
    {reply, Removed, State};
handle_msg({'$pub', Msg}, _From, State) ->
    publish_msg(Msg, State),
    {noreply, State};
handle_msg(_Msg, _From, #state{handle_msg=false}=State) ->
    {noreply, State};
handle_msg(Msg, From, #state{mod=Module, modstate=ModState0}=State) ->
    {Reply, ModState} =
        e2_service_impl:handle_msg(Module, Msg, From, ModState0),
    e2_service_impl:handle_msg_reply(Reply, State#state{modstate=ModState}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state(Module, ModState) ->
    #state{mod=Module,
           modstate=ModState,
           handle_msg=exports_handle_msg(Module),
           subs=[]}.

exports_handle_msg(Module) ->
    erlang:function_exported(Module, handle_msg, 3).

add_subscriber(Pattern, Compiled, Subscriber, #state{subs=Subs}=State) ->
    case lists:keyfind({Pattern, Subscriber}, 1, Subs) of
        false ->
            maybe_monitor(Subscriber),
            {true, State#state{subs=[{{Pattern, Subscriber}, Compiled}|Subs]}};
        _ ->
            {false, State}
    end.

maybe_monitor(Subscriber) when is_pid(Subscriber) ->
    erlang:monitor(process, Subscriber);
maybe_monitor(_) -> ok.

remove_subscriber(Pattern, Subscriber, #state{subs=Subs0}=State) ->
    Subs = lists:keydelete({Pattern, Subscriber}, 1, Subs0),
    {length(Subs0) /= length(Subs), State#state{subs=Subs}}.

remove_all_subscribers(Subscriber, #state{subs=Subs0}=State) ->
    Subs = lists:filter(fun({{_, S}, _}) when S =:= Subscriber -> false;
                           (_) -> true end, Subs0),
    {length(Subs0) /= length(Subs), State#state{subs=Subs}}.

publish_msg(_Msg, []) -> ok;
publish_msg(Msg, [{{_, Subscriber}, Compiled}|Rest]) ->
    case match_pattern(Msg, Compiled) of
        true -> try_dispatch_msg(Msg, Subscriber);
        false -> ok
    end,
    publish_msg(Msg, Rest);
publish_msg(Msg, #state{subs=Subs}) ->
    publish_msg(Msg, Subs).

compile_pattern(Pattern) ->
    ets:match_spec_compile([{Pattern, [], ['$_']}]).

match_pattern(_, all) -> true;
match_pattern(Msg, Compiled) ->
    case ets:match_spec_run([Msg], Compiled) of
        [] -> false;
        [_] -> true
    end.

try_dispatch_msg(Msg, Subscriber) ->
    try
        dispatch_msg(Msg, Subscriber)
    catch
        %% TODO: This should be a configurable policy (e.g. custom
        %% handlers, log and remove handler, etc.
        T:E ->
            ST = erlang:get_stacktrace(),
            error_logger:error_report({msg_dispatch, {T, E, ST}})
    end.

dispatch_msg(Msg, Pid) when is_pid(Pid) ->
    erlang:send(Pid, Msg);
dispatch_msg(Msg, Fun) when is_function(Fun) ->
    Fun(Msg);
dispatch_msg(Msg, {M, F}) ->
    erlang:apply(M, F, [Msg]);
dispatch_msg(Msg, {M, F, A}) ->
    erlang:apply(M, F, A ++ [Msg]).
