%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(dtls_serveur).
-export([start/0]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start() ->
    application:ensure_all_started(ssl),
    Port = 31416,
    Options = [{keyfile, "key.pem"}    % configuration du chemin vers la clé privée
             ,{certfile, "cert.pem"}  % configuration du chemin vers le certificat
             ,{ip, loopback}          % configuration de l’adresse IP sur localhost
             ,{mode, binary}          % utilisation des données au format binary
             ,{verify, verify_none}   % ne pas faire de vérification sur les certificats
             ,{active, true}         % utilisation du mode actif
              ,{protocol, dtls}],     % utilisation du protocole dtls
    {ok, SocketDTLS} = ssl:listen(Port, Options),
    {ok, SocketAccept} = ssl:transport_accept(SocketDTLS),
    {ok, SocketHandshake} = ssl:handshake(SocketAccept),
    ssl:send(SocketHandshake, <<"Le serveur DTLS fonctionne!">>),
    Message = receive {ssl, _, MessageSSL} -> MessageSSL after 10000 -> timeout end,
    ssl:close(SocketHandshake).

