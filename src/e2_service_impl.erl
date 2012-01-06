%%% Support for custom service implementations.
%%%
%%% I'm not 100% happy with this name: hearkens to Java "Impl" classes
%%% and otherwise is totally obvious. Other options considered:
%%% e2_service_util (bad, overused "util" grab bag of misc supporting
%%% functions), e2_service_cb / e2_service_cbmod / e2_service_callback
%%% (arguably better as we typically refer to these modules as "callback
%%% modules, but it feels jargony).

-module(e2_service_impl).

-export([split_options/2,
         init/2, init_reply/2,
         handle_msg/4, handle_msg_reply/2]).

%%%===================================================================
%%% API
%%%===================================================================

split_options(Module, Options) ->
    split_options(Module, Options, [], []).

init(Module, Args) ->
    case e2_util:optional_function(Module, init, 1) of
        undefined ->
            handle_init_result({ok, Args});
        Init ->
            handle_init_result(e2_util:apply_handler(Init, [Args]))
    end.

init_reply({ok, _}, State) ->
    {ok, State};
init_reply({ok, _, Timeout}, State) when is_integer(Timeout) ->
    {ok, State, Timeout};
init_reply({ok, _, FirstMsg}, State) ->
    {ok, State, FirstMsg};
init_reply({stop, Reason}, _) ->
    {stop, Reason};
init_reply(ignore, _) ->
    ignore.

handle_msg(Module, Msg, From, State) ->
    handle_msg_result(Module:handle_msg(Msg, From, State)).

handle_msg_reply({noreply, _}, State) ->
    {noreply, State};
handle_msg_reply({noreply, _, Timeout}, State) ->
    {noreply, State, Timeout};
handle_msg_reply({reply, Reply, _}, State) ->
    {reply, Reply, State};
handle_msg_reply({reply, Reply, _, Timeout}, State) ->
    {reply, Reply, State, Timeout};
handle_msg_reply({stop, Reason, _}, State) ->
    {stop, Reason, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

split_options(_Module, [], ServiceOpts, ImplOpts) ->
    {ServiceOpts, ImplOpts};
split_options(Module, [registered|Rest], ServiceOpts, ImplOpts) ->
    split_options(Module, Rest, [{registered, Module}|ServiceOpts], ImplOpts);
split_options(Module, [{registered, Name}|Rest], ServiceOpts, ImplOpts) ->
    split_options(Module, Rest, [{registered, Name}|ServiceOpts], ImplOpts);
split_options(Module, [O|Rest], ServiceOpts, ImplOpts) ->
    split_options(Module, Rest, ServiceOpts, [O|ImplOpts]).

handle_init_result({ok, State}) ->
    {{ok, State}, State};
handle_init_result({ok, State, Timeout}) when is_integer(Timeout) ->
    {{ok, State, Timeout}, State};
handle_init_result({ok, State, FirstMsg}) ->
    {{ok, State, FirstMsg}, State};
handle_init_result({stop, Reason}) ->
    {{stop, Reason}, undefined};
handle_init_result(ignore) ->
    {ignore, undefined};
handle_init_result(Other) ->
    exit({bad_return_value, Other}).

handle_msg_result({noreply, State}) ->
    {{noreply, State}, State};
handle_msg_result({noreply, State, Timeout}) ->
    {{noreply, State, Timeout}, State};
handle_msg_result({reply, Reply, State}) ->
    {{reply, Reply, State}, State};
handle_msg_result({reply, Reply, State, Timeout}) ->
    {{reply, Reply, Timeout}, State};
handle_msg_result({stop, Reason, State}) ->
    {{stop, Reason, State}, State};
handle_msg_result(Other) ->
    exit({bad_result_value, Other}).
