%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tcp_acceptor_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1, start_acceptor/1]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(_Arguments) ->
    SupervisorSpec = #{ strategy => one_for_one },
    ChildrenSpec = [],
    Supervisor = {SupervisorSpec, ChildrenSpec},
    {ok, Supervisor}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_acceptor(Socket) ->
    Spec = #{ id => {cache_tcp_acceptor, erlang:unique_integer()}
            , start => { cache_tcp_acceptor, start_link, [Socket] }
            , restart => permanent
            , type => worker
            },
    supervisor:start_child(?MODULE, Spec).
