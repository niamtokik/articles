%%%-------------------------------------------------------------------
%%% @doc cache_sup permet de superviser le module cache et un
%%% gestionnaire d'évènement.  
%%% @end
%%%-------------------------------------------------------------------
-module(cache_udp_sup).
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
init(Arguments) ->
    Port = proplists:get_value(port, Arguments, 31415),
    SupervisorConf = #{ strategy => one_for_one,
                        intensity => 1,
                        period => 5 },
    UdpArgs = [{local, cache_udp_listener}, cache_udp_listener, Port, []],
    UdpStart = {gen_server, start_link, UdpArgs},
    UdpSpec = #{ id => cache_udp_listener, start => UdpStart },
    {ok, {SupervisorConf, [UdpSpec]}}.
