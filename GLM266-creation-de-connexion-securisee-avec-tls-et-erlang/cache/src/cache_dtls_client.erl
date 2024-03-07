%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_dtls_client).
-export([add/4, delete/3, get/3, get_keys/2, get_values/2]).

add(Host, Port, Key, Value) ->
    send(Host, Port, async, [<<"add">>, Key, Value]).

delete(Host, Port, Key) ->
    send(Host, Port, async, [<<"delete">>, Key]).

get(Host, Port, Key) ->
    send(Host, Port, sync, [<<"get">>, Key]).

get_keys(Host, Port) ->
    send(Host, Port, sync, [<<"get_keys">>]).

get_values(Host, Port) ->
    send(Host, Port, sync, [<<"get_values">>]).

send(Host, Port, Type, Command) ->
    cache_tls_client:send(Host, Port, Type, [<<"get_values">>], dtls).
