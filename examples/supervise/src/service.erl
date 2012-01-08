-module(service).

-behaviour(e2_service).

-export([start_link/0, stop/1]).

-export([handle_msg/3]).

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

stop(Reason) ->
    e2_service:cast(?MODULE, {stop, Reason}).

handle_msg({stop, Reason}, noreply, State) ->
    {stop, Reason, State}.
