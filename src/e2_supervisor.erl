-module(e2_supervisor).

-behaviour(supervisor).

-export([start_link/2, start_link/3, supervisor_spec/2]).

-export([init/1]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [].

-define(DEFAULT_STRATEGY, one_for_one).

-define(DEFAULT_MAX_RESTART, {1, 1}).

-define(STRATEGIES,
        [one_for_all,
         one_for_one,
         rest_for_one,
         simple_one_for_one]).

-define(OPTIONS_SCHEMA,
        [{strategy,
          [{values, ?STRATEGIES}, implicit,
           {default, ?DEFAULT_STRATEGY}]},
         {max_restart,
          [{validate, fun validate_max_restart/1},
           {default, ?DEFAULT_MAX_RESTART}]}]).

-define(CHILD_SCHEMA,
        [{id, [optional]},
         {type,
          [{values, [worker, supervisor]},
           {default, worker},
           implicit]},
         {function, [atom, {default, start_link}]},
         {args, [list, {default, []}]},
         {restart,
          [{values, [permanent, temporary, transient]},
           {default, permanent},
           implicit]},
         {shutdown,
          [{validate, fun validate_shutdown/1},
           {default, brutal_kill}]}]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Children, Options) ->
    supervisor:start_link(?MODULE, supervisor_spec(Children, Options)).

start_link(SupName, Children, Options) when is_atom(SupName) ->
    supervisor:start_link({local, SupName}, ?MODULE,
                          supervisor_spec(Children, Options)).

supervisor_spec(Children, Options) ->
    ValidatedOpts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    {restart_spec(ValidatedOpts), child_specs(Children)}.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init(SupervisorSpec) ->
    %% NOTE: This is a straight pass through from the start_link args.
    %% I'm not sure what other operation could be performed here, but
    %% there might be a need for a callback to an initializer here (i.e.
    %% within the context of the supervisor process).
    {ok, SupervisorSpec}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_max_restart({MaxR, MaxT})
  when is_integer(MaxR), is_integer(MaxT),
       MaxR >= 0, MaxT >= 0 -> ok;
validate_max_restart(_) -> error.

validate_shutdown(Time) when is_integer(Time), Time >= 0 -> ok;
validate_shutdown(brutal_kill) -> ok;
validate_shutdown(_) -> error.

child_specs(Children) ->
    lists:map(fun child_spec/1, Children).

child_spec({Mod, Options0}) when is_atom(Mod) ->
    Opts = e2_opt:validate(Options0, ?CHILD_SCHEMA),
    Id = e2_opt:value(id, Opts, Mod),
    Fun = e2_opt:value(function, Opts),
    Args = e2_opt:value(args, Opts),
    Restart = e2_opt:value(restart, Opts),
    Shutdown = e2_opt:value(shutdown, Opts),
    Type = e2_opt:value(type, Opts),
    {Id, {Mod, Fun, Args}, Restart, Shutdown, Type, [Mod]};
child_spec(Mod) when is_atom(Mod) -> child_spec({Mod, []});
child_spec(Other) -> error({badarg, Other}).

restart_spec(Opts) ->
    {MaxR, MaxT} = e2_opt:value(max_restart, Opts),
    {e2_opt:value(strategy, Opts), MaxR, MaxT}.
