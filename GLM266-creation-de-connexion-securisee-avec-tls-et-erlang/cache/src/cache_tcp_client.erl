%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tcp_client).
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
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
    gen_tcp:send(Socket, cache_lib:join(Command)),
    case Type of
        async ->
            gen_tcp:close(Socket);
        sync ->
            Return = gen_tcp:recv(Socket, 0),
            gen_tcp:close(Socket),
            Return
    end.
            
