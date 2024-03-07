%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tls_client).
-export([start/0, start/1]).
-include_lib("kernel/include/logger.hrl").
-define(HOST, localhost).
-define(PORT, 7890).
-define(MESSAGE_TCP, <<"client side tcp">>).
-define(MESSAGE_SSL, <<"client side ssl">>).
-define(TIMEOUT, 30000).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start() ->
    OptionsClient = [{mode, binary},{verify, verify_none}
                    ,{active, false},{protocol, tls}
                    ],
    start(OptionsClient).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start(OptionsClient) ->
    io:format("⓿ (~p) démarrage de l'application ssl~n", [?MODULE]),
    _ = application:ensure_all_started(ssl),

    io:format("❷ (~p) connexion a ~p~n", [?MODULE, ?HOST]),
    {ok, TCPClientSocket} = gen_tcp:connect(?HOST, ?PORT, [{active,false}]),
    timer:sleep(1000),

    io:format("❸ (~p) envois ~p~n", [?MODULE, ?MESSAGE_TCP]),
    gen_tcp:send(TCPClientSocket, ?MESSAGE_TCP),
    timer:sleep(1000),

    io:format("❸ (~p) attend la réception d'un message~n", [?MODULE]),
    MessageServeurTCP = gen_tcp:recv(TCPClientSocket, 0, ?TIMEOUT),
    io:format("❸ (~p) a reçu le message ~p~n", [?MODULE, MessageServeurTCP]),
    timer:sleep(1000),

    io:format("❹ (~p) démarrage de la poignée de main TLS~n", [?MODULE]),
    {ok, SSLClient} = ssl:connect(TCPClientSocket, OptionsClient, ?TIMEOUT),
    timer:sleep(1000),

    io:format("❺ (~p) attend la réception d'un message~n", [?MODULE]),
    MessageServeurSSL = ssl:recv(SSLClient, 0, ?TIMEOUT),
    io:format("❺ (~p) a reçu le message ~p~n", [?MODULE, MessageServeurSSL]),
    timer:sleep(1000),

    io:format("❺ (~p) envois le message ~p~n", [?MODULE, ?MESSAGE_SSL]),
    ssl:send(SSLClient, ?MESSAGE_SSL),
    timer:sleep(1000),

    io:format("❻ (~p) fermeture de la connexion TLS~n", [?MODULE]),
    ssl:close(SSLClient),
    timer:sleep(1000).
