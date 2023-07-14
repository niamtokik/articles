%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tls_serveur).
-export([start/0, start/1]).
-include_lib("kernel/include/logger.hrl").
-define(TIMEOUT, 30000).
-define(PORT, 7890).
-define(MESSAGE_TCP, <<"server side tcp">>).
-define(MESSAGE_SSL, <<"server side ssl">>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start() ->
    OptionsServeur = [{keyfile, "key.pem"},{certfile, "cert.pem"},{mode, binary}
                     ,{verify, verify_none},{active, false},{protocol, tls}
                     ],
    start(OptionsServeur).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start(OptionsServeur) ->
    io:format("⓿ (~p) démarrage de l'application ssl~n", [?MODULE]),
    _ = application:ensure_all_started(ssl),

    io:format("❶ (~p) écoute sur le port TCP/~p~n", [?MODULE, ?PORT]),
    {ok, TCPServeurSocket} = gen_tcp:listen(?PORT, [{active,false}]),
    timer:sleep(1000),

    io:format("❶ (~p) attente d'une connexion client~n", [?MODULE]),
    {ok, AcceptServeurSocket} = gen_tcp:accept(TCPServeurSocket),
    timer:sleep(1000),

    io:format("❸ (~p) attente de réception d'un message~n", [?MODULE]),
    MessageClientTCP = gen_tcp:recv(AcceptServeurSocket, 0, ?TIMEOUT),
    io:format("❸ (~p) réception du message ~p~n", [?MODULE, MessageClientTCP]),
    timer:sleep(1000),

    io:format("❸ (~p) envois du message ~p~n", [?MODULE, ?MESSAGE_TCP]),
    gen_tcp:send(AcceptServeurSocket, ?MESSAGE_TCP),
    timer:sleep(1000),

    io:format("❹ (~p) attente d'une poignée de main TLS~n", [?MODULE]),
    {ok, SSLServeur} = ssl:handshake(AcceptServeurSocket, OptionsServeur, ?TIMEOUT),
    timer:sleep(1000),

    io:format("❺ (~p) envois du message ~p~n", [?MODULE, ?MESSAGE_SSL]),
    ssl:send(SSLServeur, ?MESSAGE_SSL),
    timer:sleep(1000),
    
    io:format("❺ (~p) attend la réception d'un message~n", [?MODULE]),
    MessageClientSSL = ssl:recv(SSLServeur, 0, ?TIMEOUT),
    io:format("❺ (~p) a reçu le message ~p~n", [?MODULE, MessageClientSSL]),
    timer:sleep(1000),
    
    io:format("❻ (~p)~n", [?MODULE]),
    ssl:close(SSLServeur),
    timer:sleep(1000),

    io:format("❼ (~p) fermeture du socket TCP~n", [?MODULE]),
    gen_tcp:close(TCPServeurSocket),
    timer:sleep(1000).
