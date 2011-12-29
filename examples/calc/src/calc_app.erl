-module(calc_app).

-behavior(e2_application).

-export([init/0]).

init() -> {ok, [{calc_handler_sup, [supervisor]}, calc_server]}.

