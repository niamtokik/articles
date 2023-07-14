%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(Arguments, Options) -> Resultat when
      Arguments :: term(),
      Options :: term(),
      Resultat :: {ok, pid()}.
start(_Arguments, _Options) ->
    cache_sup:start_link().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop(Etat) -> Resultat when
      Etat :: term(),
      Resultat :: ok.
stop(_Etat) ->
  ok.
