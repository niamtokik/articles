-module(cache_tls_listener).
-export([callback_mode/0, init/1, terminate/3]).
-export([active/3]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").

callback_mode() -> state_functions.

init(_Arguments) ->
    Env = application:get_env(cache, ssl, []),
    Port = proplists:get_value(port, Env, 8890),
    Acceptors = proplists:get_value(acceptors, Env, 10),
    Certificat = proplists:get_value(certfile, Env, "cert.pem"),
    Cle = proplists:get_value(keyfile, Env, "key.pem"),
    Protocol = proplists:get_value(protocol, Env, tls),
    Verify = proplists:get_value(verify, Env, verify_peer),
    Options = [{active, true}
              ,{mode, binary}
              ,{keyfile, Cle}
              ,{certfile, Certificat}
              ,{protocol, Protocol}
              ,{verify, Verify}],
    {ok, Listener} = ssl:listen(Port, Options),
    init_acceptor(cache_tls_acceptor_sup, Listener, Acceptors),
    {ok, active, Listener}.

terminate(_Raison, _State, Listener) ->
    ssl:close(Listener).

active(MessageType, Message, Listener) ->
    {keep_state, Listener}.

start_acceptor(Supervisor, Socket) ->
    StartArgs = [cache_tls_acceptor, Socket, []],
    Spec = #{ id => {cache_tls_acceptor, erlang:unique_integer()}
            , start => { gen_server, start_link, StartArgs }
            , restart => permanent
            , type => worker
            },
    supervisor:start_child(Supervisor, Spec).

init_acceptor(Supervisor, Socket, Acceptors) ->
    [ start_acceptor(Supervisor, Socket) || _ <- lists:seq(0, Acceptors) ].
