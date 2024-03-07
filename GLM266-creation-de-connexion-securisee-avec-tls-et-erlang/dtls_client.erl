%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(dtls_client).
-export([start/0]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start() ->
    {ok, _} = application:ensure_all_started(ssl),
    Options = [{mode, binary}          % utilisation des données au format binary
              ,{verify, verify_none}   % ne pas faire de vérification sur les certificats
              ,{active, true}         % utilisation du mode actif
              ,{protocol, dtls}],     % utilisation du protocole dtls
    {ok, SocketSSL} = ssl:connect(localhost, 31416, Options),
    ssl:send(SocketSSL, Message),
    Message = receive {ssl, _, MessageSSL} -> MessageSSL after 10000 -> timeout end,
    io:format("~p~n", [Message]),
    ssl:close(SocketSSL).
    
