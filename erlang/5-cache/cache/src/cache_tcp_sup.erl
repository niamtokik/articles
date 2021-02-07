%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tcp_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link(Arguments) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Arguments).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Arguments) ->
    SupervisorSpec = #{ strategy => one_for_one },
    AcceptorSup = #{ id => cache_tcp_acceptor_sup
                   , start => {cache_tcp_acceptor_sup, start_link, []}
                   , restart => permanent
                   , type => supervisor
                   },
    Listener = #{ id => cache_tcp_listener
                , start => {cache_tcp_listener, start_link, [[]]} 
                , restart => permanent
                , type => worker
                },
    ChildrenSpec = [AcceptorSup, Listener],
    Supervisor = {SupervisorSpec, ChildrenSpec},
    {ok, Supervisor}.
