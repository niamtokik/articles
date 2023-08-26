-module(cache_http_client).
-export([add/4, delete/3, get/3, check/3]).

scheme() -> "http".
type() -> "text/plain".
headers() -> [].    

url(Host, Port, Key) ->
    uri_string:recompose(#{ 
      port => Port,
      scheme => scheme(),
       path => "/" ++ Key,
       host => Host
     }).

request(Method, Request) ->
    case httpc:request(Method, Request, [], []) of
	{ok, {{_,400,_},_,_}} ->
	    {error, bad_request};
	{ok, {{_,404,_},_,_}} ->
	    {error, not_found};
	{ok, {{_,200,_},_, Data}} ->
	    {ok, list_to_binary(Data)};
	Elsewise ->
	    {error, Elsewise}
    end.

add(Host, Port, Key, Value) 
  when is_binary(Key) ->
    add(Host, Port, binary_to_list(Key), Value);
add(Host, Port, Key, Value) 
  when is_binary(Value) ->
    add(Host, Port, Key, binary_to_list(Value));
add(Host, Port, Key, Value) ->
    Url = url(Host, Port, Key),
    request(put, {Url, headers(), type(), Value}).

delete(Host, Port, Key) 
  when is_binary(Key) ->
    delete(Host, Port, binary_to_list(Key));
delete(Host, Port, Key) ->
    Url = url(Host, Port, Key),
    request(delete, {Url, headers()}).

get(Host, Port, Key) 
  when is_binary(Key) ->
    get(Host, Port, binary_to_list(Key));
get(Host, Port, Key) ->
    Url = url(Host, Port, Key),
    request(get, {Url, headers()}).

check(Host, Port, Key) 
  when is_binary(Key) ->
    check(Host, Port, binary_to_list(Key));
check(Host, Port, Key) ->
    Url = url(Host, Port, Key),
    request(head, {Url, headers()}).
    
