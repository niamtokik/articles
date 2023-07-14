%%%-------------------------------------------------------------------
%%% @doc cache_event est un module permettant de montrer le
%%% fonctionnement d'un event handler utilisant le behaviour
%%% gen_event.
%%% @end
%%%-------------------------------------------------------------------
-module(cache_event).
-behaviour(gen_event).
-export([init/1]).
-export([handle_event/2]).

%%--------------------------------------------------------------------
%% @doc init/1 est un callback obligatoire. Dans ce cas, il configure
%% simplement l'état avec une liste nulle 
%% @end
%%--------------------------------------------------------------------
init(_) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc handle_event/2 récupère un évènement et affiche simplement un
%% message sur la sortie standard
%% @end
%%--------------------------------------------------------------------
handle_event(Event, State) ->
    io:format("receive: ~p~n", [Event]),
    {ok, State}.

