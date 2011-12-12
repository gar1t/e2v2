-module(e2_opt).

-export([validate/2, validate/3, value/2]).

-define(NO_DEFAULT, '$e2_opt_nodefault').

-record(schema, {constraints}).
-record(constraint, {values, type, min, max, pattern, default=?NO_DEFAULT}).

-define(is_type(T), (T == int orelse
                     T == float orelse
                     T == string orelse
                     T == number)).

%%%===================================================================
%%% API
%%%===================================================================

validate(Options, Schema) ->
    validate(Options, compile_schema(Schema), dict:new()).

validate([], #schema{}=Schema, Opts0) ->
    apply_missing(Schema, Opts0);
validate([Opt|Rest], #schema{}=Schema, Opts0) ->
    validate(Rest, Schema, apply_opt(Opt, Schema, Opts0));
validate(MoreOptions, Schema, Opts0) ->
    validate(MoreOptions, compile_schema(Schema), Opts0).

value(Name, Opts) -> dict:fetch(Name, Opts).

compile_schema(Schema) ->
    #schema{constraints=[compile_constraint(C) || C <- Schema]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_constraint(Name) when is_atom(Name) ->
    {Name, #constraint{}};
compile_constraint({Name, Opts}) ->
    {Name, apply_constraint_options(Opts, #constraint{})}.

-define(constraint_val(Field, Val, C), C#constraint{Field=Val}).

apply_constraint_options([], C) -> C;
apply_constraint_options([{values, Values}|Rest], C) when is_list(Values) ->
    apply_constraint_options(Rest, ?constraint_val(values, Values, C));
apply_constraint_options([{type, Type}|Rest], C) when ?is_type(Type) ->
    apply_constraint_options(Rest, ?constraint_val(type, Type, C));
apply_constraint_options([{min, Min}|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(min, Min, C));
apply_constraint_options([{max, Max}|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(max, Max, C));
apply_constraint_options([{pattern, Pattern}|Rest], C) ->
    case re:compile(Pattern) of
        {ok, Re} ->
            apply_constraint_options(Rest, ?constraint_val(pattern, Re, C));
        {error, _} ->
            error({option, pattern})
    end;
apply_constraint_options([{default, Default}|Rest], C) ->
    apply_constraint_options(Rest, ?constraint_val(default, Default, C));
apply_constraint_options([{Name, _}|_], _) ->
    error({option, Name});
apply_constraint_options([Other|_], _) ->
    error({option, Other}).

apply_opt(Opt, Schema, Opts) ->
    {Name, Value} = validate_opt(Opt, Schema),
    case dict:find(Name, Opts) of
        {ok, _} -> error({duplicate_option, Name});
        error -> dict:store(Name, Value, Opts)
    end.

validate_opt({Name, Val}, Schema) ->
    case find_constraint(Name, Schema) of
        {ok, Constraint} ->
            case check_value(Val, Constraint) of
                ok -> {Name, Val};
                error -> error({value, Name})
            end;
        error -> error({option, Name})
    end;
validate_opt(Name, Schema) ->
    validate_opt({Name, true}, Schema).

find_constraint(Name, #schema{constraints=Constraints}) ->
    case lists:keyfind(Name, 1, Constraints) of
        {Name, Constraint} -> {ok, Constraint};
        false -> error
    end.

check_value(Val, Constraint) ->
    apply_checks(Val, Constraint,
                 [fun check_enum/2,
                  fun check_type/2,
                  fun check_range/2,
                  fun check_pattern/2]).

apply_checks(_Val, _Constraint, []) -> ok;
apply_checks(Val, Constraint, [Check|Rest]) ->
    case Check(Val, Constraint) of
        ok -> apply_checks(Val, Constraint, Rest);
        error -> error
    end.


check_enum(_Val, #constraint{values=undefined}) -> ok;
check_enum(Val, #constraint{values=Values}) ->
    case lists:member(Val, Values) of
        true -> ok;
        false -> error
    end.

-define(is_iolist(T),
        try erlang:iolist_size(Val) of
            _ -> true
        catch
            error:badarg -> false
        end).

check_type(_Val, #constraint{type=undefined}) -> ok;
check_type(Val, #constraint{type=int}) when is_integer(Val) -> ok;
check_type(Val, #constraint{type=float}) when is_float(Val) -> ok;
check_type(Val, #constraint{type=number}) when is_number(Val) -> ok;
check_type(Val, #constraint{type=string}) ->
    case ?is_iolist(Val) of
        true -> ok;
        false -> error
    end;
check_type(_, _) -> error.

check_range(_Val, #constraint{min=undefined, max=undefined}) -> ok;
check_range(Val, #constraint{min=undefined, max=Max}) when Val =< Max -> ok;
check_range(Val, #constraint{min=Min, max=undefined}) when Val >= Min -> ok;
check_range(Val, #constraint{min=Min, max=Max}) when Val =< Max,
                                                     Val >= Min-> ok;
check_range(_, _) -> error.

check_pattern(_Val, #constraint{pattern=undefined}) -> ok;
check_pattern(Val, #constraint{pattern=Regex}) ->
    case re:run(Val, Regex, [{capture, none}]) of
        match -> ok;
        nomatch -> error
    end.

apply_missing(#schema{constraints=Constraints}, Opts0) ->
    lists:foldl(fun apply_default/2, Opts0, Constraints).

apply_default({Name, #constraint{default=Default}}, Opts) ->
    case dict:find(Name, Opts) of
        {ok, _} -> Opts;
        error ->
            case Default of
                ?NO_DEFAULT -> error({required, Name});
                _ -> dict:store(Name, Default, Opts)
            end
    end.
