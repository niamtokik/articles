-module(cache_http_server_handler).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(#mod{ method = "HEAD" } = Request) ->
    http_head(Request);
do(#mod{ method = "GET" } = Request) ->
    http_get(Request);
do(#mod{ method = "PUT" } = Request) ->
    http_put(Request);
do(#mod{ method = "POST" } = Request) ->
    http_post(Request);
do(#mod{ method = "DELETE" } = Request) ->
    http_delete(Request).

http_head(#mod{ request_uri = "/" ++ Key } = Request) ->
    Result = cache:get(cache, list_to_binary(Key)),
    case Result of
        undefined ->
            {break, [{response, {404, ""}}]};
        _ ->
            {break, [{response, {200, ""}}]}
    end.    
http_get(#mod{ request_uri = "/_/" ++ "keys" } = Request) ->
    Result = cache:get_keys(cache),
    Convert = [ binary_to_list(X) || X <- Result ],
    Output = lists:join("\n", Convert),
    {break, [{response, {200, Output}}]};
http_get(#mod{ request_uri = "/" ++ Key } = Request) ->
    logger:error("~p~n", [{Request, Key}]),
    Result = cache:get(cache, list_to_binary(Key)),
    case Result of
        undefined ->
            {break, [{response, {404, "Not found"}}]};
        _ ->
            Output = io_lib:format("~ts", [Result]),
            {break, [{response, {200, Output}}]}
    end.

http_put(#mod{ request_uri = "/" ++ Key, entity_body = Value } = Request) ->
    cache:add(cache, list_to_binary(Key), list_to_binary(Value)),
    {break, [{response, {200, "ok"}}]}.

http_post(Request) ->
    {break, [{response, {200, "post\n"}}]}.

http_delete(#mod{ request_uri = "/" ++ Key } = Request) ->
    cache:delete(cache, list_to_binary(Key)),
    {break, [{response, {200, "ok"}}]}.
