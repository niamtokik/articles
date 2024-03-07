%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tcp_sup).
-behavior(supervisor).
-export([init/1]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Arguments) ->
    SupervisorSpec = #{ strategy => one_for_one },
    
    AcceptorArgs = [{local, cache_tcp_acceptor_sup}, cache_tcp_acceptor_sup, []], 
    AcceptorSup = #{ id => cache_tcp_acceptor_sup
                   , start => {supervisor, start_link, AcceptorArgs}
                   , restart => permanent
                   , type => supervisor
                   },

    ListenerArgs = [{local, cache_tcp_listener}, cache_tcp_listener, [], []],
    Listener = #{ id => cache_tcp_listener
                , start => {gen_statem, start_link, ListenerArgs }
                , restart => permanent
                , type => worker
                },

    ChildrenSpec = [AcceptorSup,Listener],
    Supervisor = {SupervisorSpec, ChildrenSpec},
    {ok, Supervisor}.
