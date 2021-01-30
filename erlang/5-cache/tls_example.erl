%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%%
%%%  avant d'exécuter ce code, il est nécessaire d'exécuter les
%%%  les commandes suivantes pour générer un certificat valide.
%%%
%%%     #!/bin/sh
%%%     openssl req -x509 -nodes -newkey rsa:2048 \
%%%             -keyout key.pem \
%%%             -out cert.pem \
%%%             -subj '/C=SE/CN=localhost/CN=*.localhost/O=localhost'
%%%
%%%  ou
%%%
%%%     #!/bin/sh
%%%     cat > template << EOF
%%%     cn = "localhost"
%%%     dns_name = "localhost"
%%%     signing_key
%%%     encryption_key
%%%     EOF
%%%     certtool --generate-privkey --outfile key.pem
%%%     certtool --generate-self-signed --load-privkey key.pem \
%%%              --outfile cert.pem --template=template
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tls_example).
-export([server/0]).

server() ->
    io:format("démarrage de l'application ssl...~n", []),
    application:ensure_all_started(ssl),

    Port = 31415,
    Options = [{keyfile, "key.pem"}    % configuration du chemin vers la clé privée
              ,{certfile, "cert.pem"}  % configuration du chemin vers le certificat
              ,{mode, binary}          % utilisation des données au format binary
              ,{verify, verify_none}], % ne pas faire de vérification sur les certificats
    io:format("démarrage du service sur le port ~p~n", [Port]),

    {ok, SocketTLS} = ssl:listen(Port, Options),
    io:format("service démarré: ~p~n", [SocketTLS]),

    io:format("en attente de connexion...~n", []),
    {ok, SocketAccept} = ssl:transport_accept(SocketTLS),
    io:format("connexion accepté: ~p~n", [SocketAccept]),

    io:format("négociation TLS...~n", []),
    {ok, SocketHandshake} = ssl:handshake(SocketAccept),
    io:format("négociation terminée: ~p~n", [SocketHandshake]),

    io:format("envois d'un message au client...~n", []),
    ssl:send(SocketHandshake, <<"Le serveur TLS fonctionne!">>),

    io:format("fermeture de la connexion.~n", []),
    ssl:close(SocketHandshake),

    io:format("arret de l'application ssl"),
    application:stop(ssl).
