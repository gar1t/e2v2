-module(supervise).

-export([permanent/0, temporary/0, transient/0]).

-import(error_logger, [info_msg/1, info_msg/2]).

permanent() ->
    application:start(sasl),

    info_msg("Starting supervisor with permanent child~n"),
    Children = [{service, [permanent]}],
    SupervisorOpts = [{max_restart, {1, 1}}],
    {ok, Sup} = e2_supervisor:start_link(Children, SupervisorOpts),

    timer:sleep(100),
    info_msg("Simulating service stop (will be restarted)~n"),
    service:stop(normal),

    timer:sleep(100),
    info_msg("Simulating another service stop (won't be restarted)~n"),
    service:stop(normal),

    timer:sleep(100),
    info_msg("Stopping supervisor~n"),
    exit(Sup, shutdown),

    timer:sleep(100).

temporary() ->
    application:start(sasl),

    info_msg("Starting supervisor with temporary child~n"),
    Children = [{service, [temporary]}],
    SupervisorOpts = [{max_restart, {1, 1}}],
    {ok, Sup} = e2_supervisor:start_link(Children, SupervisorOpts),

    timer:sleep(100),
    info_msg("Simulating service error (won't be started)~n"),
    service:stop(error),

    info_msg("Stopping supervisor~n"),
    exit(Sup, shutdown),

    timer:sleep(100).

transient() ->
    application:start(sasl),

    info_msg("Starting supervisor with transient child~n"),
    Children = [{service, [transient]}],
    SupervisorOpts = [{max_restart, {2, 1}}],
    {ok, Sup} = e2_supervisor:start_link(Children, SupervisorOpts),

    timer:sleep(100),
    info_msg("Simulating service error (will be restarted)~n"),
    service:stop(error),

    timer:sleep(100),
    info_msg("Simulating server stop (will not be restarted)~n"),
    service:stop(normal),

    timer:sleep(100),
    info_msg("Stopping supervisor~n"),
    exit(Sup, shutdown),

    timer:sleep(100).
