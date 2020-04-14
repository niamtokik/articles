%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tcp_acceptor_sup).
-behavior(supervisor).
-export([init/1]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(_Arguments) ->
    SupervisorSpec = #{ strategy => one_for_one },
    ChildrenSpec = [],
    Supervisor = {SupervisorSpec, ChildrenSpec},
    {ok, Supervisor}.

