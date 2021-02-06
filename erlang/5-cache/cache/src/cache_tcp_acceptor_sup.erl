-module(cache_tcp_acceptor_sup).
-behavior(supervisor).
-export([init/1]).

init(Arguments) ->
    SupervisorSpec = #{ strategy => simple_one_for_one },
    ChildrenSpec = [#{ id => cache_tcp_acceptor
                     , start => { cache_tcp_acceptor, start_link, [] }
                     , restart => permanent
                     , type => worker
                     }
                   ],
    Supervisor = {SupervisorSpec, ChildrenSpec},
    {ok, Supervisor}.
