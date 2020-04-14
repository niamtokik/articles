%%%-------------------------------------------------------------------
%%% @doc cache_sup permet de superviser le module cache et un
%%% gestionnaire d'évènement.  
%%% @end
%%%-------------------------------------------------------------------
-module(cache_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

%%--------------------------------------------------------------------
%% @doc start_link/0 se base sur la fonction supervisor:start_link/2
%% pour démarrer un processus basé sur le module cache_sup.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc init/1 est un callback obligatoire. Dans ce cas là, 2 enfants
%% sont créés, un pour cache_event et un autre pour le module cache.
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    SupervisorConf = #{ strategy => one_for_one,
                        intensity => 1,
                        period => 5 },
    % EventStart = {gen_event, start_link, [{local, cache_event}, []]},
    % EventSpec = #{ id => cache_event, start => EventStart },
    CacheStart = {gen_server, start_link, [{local, cache}, cache, [], []]},
    CacheSpec = #{ id => cache, start => CacheStart },
    {ok, {SupervisorConf, [CacheSpec]}}.
