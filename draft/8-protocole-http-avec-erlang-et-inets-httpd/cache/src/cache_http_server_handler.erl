-module(cache_http_server_handler).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(#mod{ method = "HEAD", request_uri = "/" ++ Key }) ->
    http_head(list_to_binary(Key));
do(#mod{ method = "GET", request_uri = "/" ++ Key }) ->
    http_get(list_to_binary(Key));
do(#mod{ method = "PUT", request_uri = "/" ++ Key, entity_body = Value }) ->
    http_put(list_to_binary(Key), list_to_binary(Value));
do(#mod{ method = "DELETE", request_uri = "/" ++ Key }) ->
    http_delete(list_to_binary(Key));
do(_Request) ->
    {break, [{response, {400, ""}}]}.

http_head(Key) ->
    Result = cache:get(cache, Key),
    case Result of
        undefined ->
            {break, [{response, {404, ""}}]};
        Data ->
            {break, [{response, {200, ""}}]}
    end.

http_get(Key) ->
    Result = cache:get(cache, Key),
    case Result of
        undefined ->
            {break, [{response, {404, ""}}]};
        _ ->
            {break, [{response, {200, binary_to_list(Result)}}]}
    end.

http_put(Key, Value) ->
    cache:add(cache, Key, Value),
    {break, [{response, {200, ""}}]}.

http_delete(Key) ->
    cache:delete(cache, Key),
    {break, [{response, {200, ""}}]}.
