-module(calc_server).

-behavior(e2_service).

-export([start_link/0, start_link/1]).

-export([init/1, handle_info/2]).

-record(state, {socket}).

-define(TCP_OPTIONS, [binary, {active, false}, {reuseaddr, true}]).

-define(DEFAULT_PORT, 6666).

%%%-------------------------------------------------------------------
%%% Initialization
%%%-------------------------------------------------------------------

start_link() ->
    start_link(application:get_all_env()).

start_link(Options) ->
    e2_service:start_link(?MODULE, Options).

%%%-------------------------------------------------------------------
%%% Callbacks
%%%-------------------------------------------------------------------

init(Options) ->
    {ok, Socket} = gen_tcp:listen(listen_port(Options), ?TCP_OPTIONS),
    {ok, #state{socket=Socket}, 0}.

handle_info(timeout, #state{socket=LSocket}=State) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    {ok, _} = calc_handler_sup:start_handler(Socket),
    {noreply, State, 0}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

listen_port(Options) ->
    proplists:get_value(port, Options, ?DEFAULT_PORT).
