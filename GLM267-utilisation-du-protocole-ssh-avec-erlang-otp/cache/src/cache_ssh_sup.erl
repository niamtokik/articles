-module(cache_ssh_sup).
-behavior(supervisor).
-export([init/1]).

init(Arguments) ->
    SupervisorSpec = #{ strategy => one_for_one },

    StoreArgs = [],
    Store = #{ id => cache_ssh_server_key_store
             , start => {cache_ssh_server_key_store, start_link, StoreArgs}
             , restart => permanent
             , type => worker
             },

    DaemonArgs = [22222, [{key_cb, cache_ssh_server_key_handler}
                         ,{shell, disabled}
                         ,{ssh_cli, {cache_ssh_server_handler, []}}
                         ,{subsystems, [{"cache", {cache_ssh_server_handler, []}}]}
                         ,{exec, disabled}
                         ]
                 ],
    Daemon = #{ id => cache_ssh_daemon
              , start => {ssh, daemon, DaemonArgs}
              , restart => permanent
              , type => supervisor
              },

    ChildrenSpec = [Store, Daemon],
    Supervisor = {SupervisorSpec, ChildrenSpec},
    {ok, Supervisor}.
