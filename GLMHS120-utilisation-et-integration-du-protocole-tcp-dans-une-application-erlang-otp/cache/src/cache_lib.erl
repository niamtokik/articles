%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_lib).
-export([parse/1, parse/2]).
-export([join/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
parse(Message) ->
    parse(Message, cache).

parse(Message, Application) ->
    ?LOG_DEBUG("get message: ~p", [Message]),
    Clean = re:replace(Message, <<"\n">>, <<>>, [global,{return, binary}]),
    case re:split(Clean, <<"\s+">>, [{parts, 3}]) of
        [<<"get">>, Key] ->
            ?LOG_DEBUG("get key ~p", [Key]),
            {cache, get, [Application,Key]};
        [<<"add">>, Key, Value] -> 
            ?LOG_DEBUG("add key ~p with value ~p", [Key, Value]),
            {cache, add, [Application,Key, Value]};
        [<<"del">>, Key] -> 
            ?LOG_DEBUG("del key ~p", [Key]),
            {cache, del, [Application,Key]};
        [<<"list">>] -> 
            ?LOG_DEBUG("list keys"),
            {cache, list, [Application]};
        [<<"get_keys">>] -> 
            ?LOG_DEBUG("get_keys"),
            {cache, get_keys, [Application]};
        [<<"get_values">>] -> 
            ?LOG_DEBUG("get_values"),
            {cache, get_values, [Application]};
        _ ->
            ?LOG_DEBUG("wrong command"),
            {error, <<"wrong command">>}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
join(List) ->
    join(List, <<>>).

join([], Buffer) ->
    Buffer;
join([L], Buffer) ->
    <<Buffer/bitstring, L/bitstring>>;
join([H|T], Buffer) ->
    join(T, <<Buffer/bitstring, H/bitstring, " ">>).
