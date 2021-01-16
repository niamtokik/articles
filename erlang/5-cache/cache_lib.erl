-module(cache_lib).
-export([command/1]).
-include_lib("kernel/include/logger.hrl").

parse(Message) ->
    ?LOG_DEBUG("get message: ~p", [Message]),
    case re:split(Message, <<"\s+">>, [{parts, 3}]) of
        [<<"get">>, Key] ->
            ?LOG_DEBUG("get key ~p", [Key]),
            {get, Key};
        [<<"add">>, Key, Value] -> 
            ?LOG_DEBUG("add key ~p with value ~p", [Key, Value]),
            {add, Key, Value};
        [<<"del">>, Key] -> 
            ?LOG_DEBUG("del key ~p", [Key]),
            {del, Key};
        [<<"list">>] -> 
            ?LOG_DEBUG("list keys"),
            {list};
        _ ->
            ?LOG_DEBUG("wrong command"),
            {error, <<"wrong command">>}
    end.

command({udp, Process, Source, Port, Message}) ->
    case parse(Message) of
        {get, Key} -> gen_udp:send(Process, Source, Port, Key);
        {add, Key, Value} -> ok;
        {del, Key} -> ok;
        {list} -> gen_udp:send(Process, Source, Port, <<>>);
        {error, Reason} -> gen_udp:send(Process, Source, Port, Reason)
    end;
command({tcp, Port, Message} = Data) ->
    case parse(Message) of
        {get, Key} -> gen_tcp:send(Port, Key);
        {add, Key, Value} -> ok;
        {del, Key} -> ok;
        {list} -> gen_tcp:send(Port, <<>>);
        {error, Reason} -> gen_tcp:send(Port, Reason)
    end.

