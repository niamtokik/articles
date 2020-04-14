-module(cache_app).
-behaviour(application).
-export([start/2, stop/1]).

start(Arguments, _Options) ->
  supervisor:start_link(cache_sup, []).

stop(_Etat) ->
  ok.
