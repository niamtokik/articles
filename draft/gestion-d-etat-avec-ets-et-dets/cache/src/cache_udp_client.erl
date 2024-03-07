-module(cache_udp_client).
-export([send_async/3, send_sync/3, send_sync/4]).
-export([add/4, delete/3, get/3, get_keys/2, get_values/2]).
-include_lib("kernel/include/logger.hrl").

send_async(Host, Port, Message) ->
    {ok, P} = gen_udp:open(source_port(), [binary]),
    gen_udp:send(P, Host, Port, Message),
    gen_udp:close(P).

send_sync(Host, Port, Message) ->
    send_sync(Host, Port, Message, 3000).

send_sync(Host, Port, Message, Timeout) ->
    {ok, P} = gen_udp:open(source_port(), [binary]),    
    gen_udp:send(P, Host, Port, Message),
    receive 
        {udp, P, Address, Port, Answer} -> 
            gen_udp:close(P),
            {ok, Answer}
    after
        Timeout -> 
            gen_udp:close(P),
            {error, timeout}
    end.

add(Host, Port, Key, Value) ->
    send_async(Host, Port, join([<<"add">>, Key, Value])).

delete(Host, Port, Key) ->
    send_async(Host, Port, join([<<"delete">>, Key])).

get(Host, Port, Key) ->
    send_sync(Host, Port, join([<<"get">>, Key])).

get_keys(Host, Port) ->
    send_sync(Host, Port, [<<"get_keys">>]).

get_values(Host, Port) ->
    send_sync(Host, Port, [<<"get_values">>]).

source_port() ->
    1024+ceil(rand:uniform()*64511).

join(List) ->
    join(List, <<>>).

join([], Buffer) ->
    Buffer;
join([L], Buffer) ->
    <<Buffer/bitstring, L/bitstring>>;
join([H|T], Buffer) ->
    join(T, <<Buffer/bitstring, H/bitstring, " ">>).
