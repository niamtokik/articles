%%%-------------------------------------------------------------------
%%% @doc exemple_supervisor permet de démarrer 2 processus,
%%% exemple_statem et exemple_server. La stratégie utilisé est
%%% one_for_one, si un des 2 processus crash, il sera alors redémarré
%%% automatiquement.
%%% @end
%%%-------------------------------------------------------------------
-module(exemple_supervisor).
-export([init/1]).
-export([start_link/0]).

%%--------------------------------------------------------------------
%% @doc start_link/0 permet d'offrir une facilité de démarrage. Pour
%% l'exécuter et démarrer ce module, rien de plus simple que de faire
%% exemple_supervisor:start_link().
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc init/1 est un callback utilisé pour configuré le
%% superviseur. Il permet de configuré la spécification du superviseur
%% ainsi que la liste des spécifications des enfants à superviser.
%% @end
%%--------------------------------------------------------------------
init(Arguments) ->
    SupervisorConf = #{ strategy => one_for_one,
                        intensity => 1,
                        period => 5 },
    SpecStatem = #{ id => exemple_statem
                  , start => {exemple_statem, start_link, []}},
    SpecServer = #{ id => exemple_server
                  , start => {exemple_server, start_link, []}},
    {ok, {SupervisorConf, [SpecStatem, SpecServer]}}.
