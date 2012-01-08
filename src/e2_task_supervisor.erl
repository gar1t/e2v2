-module(e2_task_supervisor).

-behaviour(supervisor).

-export([start_link/2, start_link/3, start_task/2]).

-export([init/1]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [].

-define(DEFAULT_MAX_RESTART, {1, 1}).

-define(OPTIONS_SCHEMA,
        [{max_restart,
          [{validate, fun validate_max_restart/1},
           {default, ?DEFAULT_MAX_RESTART}]},
         {registered, [{default, undefined}]}]).

-define(CHILD_SCHEMA,
        [{function, [atom, {default, start_link}]},
         {base_args, [list, {default, []}]},
         {restart,
          [{values, [permanent, temporary, transient]},
           {default, temporary},
           implicit]},
         {shutdown,
          [{validate, fun validate_shutdown/1},
           {default, brutal_kill}]}]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Module, ChildOrArgs) ->
    start_link(Module, ChildOrArgs, []).

start_link(Module, ChildOrArgs, Options) ->
    case exports_init(Module) of
        true ->
            start_supervisor_with_init(Module, ChildOrArgs, Options);
        false ->
            start_supervisor_with_child(Module, ChildOrArgs, Options)
    end.

start_task(Sup, ExtraArgs) ->
    supervisor:start_child(Sup, ExtraArgs).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init({Module, Args}) ->
    dispatch_init(Module, Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================

exports_init(Module) ->
    erlang:function_exported(Module, init, 1).

start_supervisor_with_init(Module, Args, Options) ->
    e2_supervisor:start_link(?MODULE, {Module, Args}, Options).

start_supervisor_with_child(Module, Child, Options) ->
    e2_supervisor:start_link(Module, children(Child), sup_options(Options)).

validate_max_restart({MaxR, MaxT})
  when is_integer(MaxR), is_integer(MaxT),
       MaxR >= 0, MaxT >= 0 -> ok;
validate_max_restart(_) -> error.

validate_shutdown(Time) when is_integer(Time), Time >= 0 -> ok;
validate_shutdown(brutal_kill) -> ok;
validate_shutdown(_) -> error.

children({Mod, Options}) when is_atom(Mod) ->
    Opts = e2_opt:validate(Options, ?CHILD_SCHEMA),
    [{Mod, [{function, e2_opt:value(function, Opts)},
            {args, e2_opt:value(base_args, Opts)},
            {restart, e2_opt:value(restart, Opts)},
            {shutdown, e2_opt:value(shutdown, Opts)}]}];
children(Mod) when is_atom(Mod) ->
    children({Mod, []}).

sup_options(Options) ->
    e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    [simple_one_for_one|Options].

dispatch_init(Module, Args) ->
    handle_init_result(Module:init(Args)).

handle_init_result({ok, Child, Options}) ->
    {ok, children(Child), sup_options(Options)};
handle_init_result(ignore) -> ignore;
handle_init_result(Other) -> error({bad_return_value, Other}).
