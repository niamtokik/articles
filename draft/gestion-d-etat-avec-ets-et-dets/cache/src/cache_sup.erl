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

    % cache feature
    CacheStart = {cache, start_link, [ets]},
    CacheSpec = #{ id => cache, start => CacheStart },

    % udp feature
    UdpArgs = [{local, cache_udp_sup}, cache_udp_sup, []],
    UdpStart = {supervisor, start_link, UdpArgs},
    UdpSpec = #{ id => cache_udp_sup, start => UdpStart, type => supervisor },

    % tcp feature
    TcpArgs = [{local, cache_tcp_sup}, cache_tcp_sup, []],
    TcpStart = {supervisor, start_link, TcpArgs},
    TcpSpec = #{ id => cache_tcp_sup, start => TcpStart, type => supervisor },

    % tls feature
    TlsArgs = [{local, cache_tls_sup}, cache_tls_sup, []],
    TlsStart = {supervisor, start_link, TlsArgs},
    TlsSpec = #{ id => cache_tls_sup, start => TlsStart, type => supervisor },

    % ssh featyre
    SshArgs = [{local, cache_ssh_sup}, cache_ssh_sup, []],
    SshStart = {supervisor, start_link, SshArgs},
    SshSpec = #{ id => cache_ssh_sup, start => SshStart, type => supervisor },
    
    {ok, {SupervisorConf, [CacheSpec, UdpSpec, TcpSpec, TlsSpec, SshSpec]}}.
