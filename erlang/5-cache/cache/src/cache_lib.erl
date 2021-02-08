%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_lib).
-export([parse/1, parse/2]).
-include_lib("kernel/include/logger.hrl").

parse(Message) ->
    parse(Message, cache).

parse(Message, Application) ->
    ?LOG_DEBUG("get message: ~p", [Message]),
    Clean = re:replace(Message, <<"\n">>, <<>>, [global,{return, binary}]),
    case re:split(Clean, <<"\s+">>, [{parts, 3}]) of
        [<<"get">>, Key] ->
            ?LOG_DEBUG("get key ~p", [Key]),
            {get, [Application,Key]};
        [<<"add">>, Key, Value] -> 
            ?LOG_DEBUG("add key ~p with value ~p", [Key, Value]),
            {add, [Application,Key, Value]};
        [<<"del">>, Key] -> 
            ?LOG_DEBUG("del key ~p", [Key]),
            {del, [Application,Key]};
        [<<"list">>] -> 
            ?LOG_DEBUG("list keys"),
            {list, [Application]};
        [<<"get_keys">>] -> 
            ?LOG_DEBUG("get_keys"),
            {get_keys, [Application]};
        [<<"get_values">>] -> 
            ?LOG_DEBUG("get_values"),
            {get_values, [Application]};
        _ ->
            ?LOG_DEBUG("wrong command"),
            {error, <<"wrong command">>}
    end.
