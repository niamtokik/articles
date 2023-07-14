-module(cache_ssh_sup).
-behavior(supervisor).
-export([init/1]).

init(Arguments) ->
    SupervisorSpec = #{ strategy => one_for_one },
    {ok, {SupervisorSpec, [store(), daemon()]}}.

store() ->
    #{ id => cache_ssh_server_key_store
     , start => {cache_ssh_server_key_store, start_link, []} % 🅔
     , restart => permanent
     , type => worker }.

daemon() ->
    SshOpts = application:get_env(cache, ssh, []), % 🅕
    DaemonPort = proplists:get_value(port, SshOpts, 22222), % 🅖
    DaemonArgs = [DaemonPort, [{key_cb, cache_ssh_server_key_handler}
                              ,{shell, disabled}
                              ,{ssh_cli, {cache_ssh_server_handler, []}}
                              ,{subsystems, [{"cache", {cache_ssh_server_handler, []}}]}
                              ,{exec, disabled}]], % 🅗
    #{ id => cache_ssh_daemon
     , start => {ssh, daemon, DaemonArgs} % 🅘
     , restart => permanent
     , type => supervisor }.
