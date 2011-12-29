-module(e2_util).

-export([optional_function/3,
         required_function/3,
         apply_handler/2]).

optional_function(Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true -> {Module, Function};
        false -> undefined
    end.

required_function(Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true -> {Module, Function};
        false -> exit({required_export, {Module, Function, Arity}})
    end.

apply_handler({Module, Function, BaseArgs}, ExtraArgs) ->
    erlang:apply(Module, Function, BaseArgs ++ ExtraArgs);
apply_handler({Module, Function}, Args) ->
    erlang:apply(Module, Function, Args);
apply_handler(Fun, Args) ->
    erlang:apply(Fun, Args).
