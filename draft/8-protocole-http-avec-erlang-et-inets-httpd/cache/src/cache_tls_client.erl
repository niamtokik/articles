%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tls_client).
-export([add/4, delete/3, get/3, get_keys/2, get_values/2, send/4, send/5]).
-behavior(gen_server).

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
    send(Host, Port, Type, Command, tls).

send(Host, Port, Type, Command, Protocol) 
  when Protocol =:= tls orelse Protocol =:= dtls ->
    application:ensure_all_started(ssl),
    {ok, Socket} = ssl:connect(Host, Port, [binary, {active, false}, {protocol, Protocol}]),
    ssl:send(Socket, cache_lib:join(Command)),
    case Type of
        async ->
            ssl:close(Socket);
        sync ->
            Return = ssl:recv(Socket, 0, 10000),
            gen_tcp:close(Socket),
            Return
    end.
