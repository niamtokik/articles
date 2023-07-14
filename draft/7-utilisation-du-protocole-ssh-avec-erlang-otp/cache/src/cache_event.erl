%%%-------------------------------------------------------------------
%%% @doc cache_event est un module permettant de montrer le
%%% fonctionnement d'un event handler utilisant le behaviour
%%% gen_event.
%%% @end
%%%-------------------------------------------------------------------
-module(cache_event).
-behaviour(gen_event).
-export([init/1]).
-export([handle_event/2, handle_call/2]).

%%--------------------------------------------------------------------
%% @doc init/1 est un callback obligatoire. Dans ce cas, il configure
%% simplement l'état avec une liste nulle 
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Resultat when
      Args :: term(),
      Resultat :: {ok, []}.
init(_) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc handle_event/2 récupère un évènement et affiche simplement un
%% message sur la sortie standard
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event, State) -> Resultat when
      Event :: term(),
      State :: [],
      Resultat :: {ok, State}.
handle_event(Event, State) ->
    io:format("receive: ~p~n", [Event]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Requete, Etat) -> Resultat when
      Requete :: term(),
      Etat :: [],
      Resultat :: {ok, ok, Etat}.
handle_call(_, Etat) ->
    {ok, ok, Etat}.
