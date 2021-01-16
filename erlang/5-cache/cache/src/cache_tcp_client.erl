%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tcp_client).
-export([start/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).

start(_) ->
    ok.

init(_) ->
    ok.

terminate(_,_) ->
    ok.

handle_cast(_,_) ->
    ok.

handle_call(_,_,_) ->
    ok.

handle_info(_,_) ->
    ok.
