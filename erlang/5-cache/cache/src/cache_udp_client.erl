-module(cache_udp_client).
-compile(export_all).

send_async(Host, Port, Message) ->
    {ok, P} = gen_udp:open(source_port(), [binary]),
    gen_udp:send(P, Host, Port, Message),
    gen_udp:close(P).

send_sync(Host, Port, Message) ->
    {ok, P} = gen_udp:open(source_port(), [binary]),    
    gen_udp:send(P, Host, Port, Message),
    receive 
        {udp, P, Address, Port, Answer} -> 
            gen_udp:close(P),
            {ok, Answer}
    after
        3000 -> 
            gen_udp:close(P),
            {error, timeout}
    end.

source_port() ->
    1024+ceil(rand:uniform()*64511).

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

join(List) ->
    join(List, <<>>).

join([], Buffer) ->
    Buffer;
join([L], Buffer) ->
    <<Buffer/bitstring, L/bitstring>>;
join([H|T], Buffer) ->
    join(T, <<Buffer/bitstring, H/bitstring, " ">>).
