-module(broker).

-behavior(e2_publisher).

-export([start_link/0,
         subscribe/1, subscribe/2,
         unsubscribe/1, unsubscribe/2,
         unsubscribe_all/0, unsubscribe_all/1,
         publish/1]).

start_link() ->
    e2_publisher:start_link(?MODULE, [], [registered]).

subscribe(Pattern) ->
    e2_publisher:subscribe(?MODULE, Pattern).

subscribe(Pattern, Subscriber) ->
    e2_publisher:subscribe(?MODULE, Pattern, Subscriber).

unsubscribe(Pattern) ->
    e2_publisher:unsubscribe(?MODULE, Pattern).

unsubscribe(Pattern, Subscriber) ->
    e2_publisher:unsubscribe(?MODULE, Pattern, Subscriber).

unsubscribe_all() ->
    e2_publisher:unsubscribe_all(?MODULE).

unsubscribe_all(Subscriber) ->
    e2_publisher:unsubscribe_all(?MODULE, Subscriber).

publish(Msg) ->
    e2_publisher:publish(?MODULE, Msg).
