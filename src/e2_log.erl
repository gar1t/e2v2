-module(e2_log).

-export([info/1, info/2, error/1, error/2, tty/1]).

info(Msg) ->
    error_logger:info_report(Msg).

info(Msg, Args) ->
    error_logger:info_report(io_lib:format(Msg, Args)).

error(Msg) ->
    error_logger:error_report(Msg).

error(Msg, Args) ->
    error_logger:error_report(io_lib:format(Msg, Args)).

tty(Flag) ->
    error_logger:tty(Flag).
