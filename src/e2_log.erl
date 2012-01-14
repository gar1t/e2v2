-module(e2_log).

-export([info/1, info/2, info_report/1,
         error/1, error/2, error_report/1]).

info(Msg) ->
    error_logger:info_msg(Msg).

info(Msg, Args) ->
    error_logger:info_msg(Msg, Args).

info_report(Report) ->
    error_logger:info_report(Report).

error(Msg) ->
    error_logger:error_msg(Msg).

error(Msg, Args) ->
    error_logger:error_msg(Msg, Args).

error_report(Report) ->
    error_logger:error_report(Report).
