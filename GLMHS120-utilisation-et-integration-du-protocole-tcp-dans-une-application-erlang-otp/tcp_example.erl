%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_example).
-export([client/0]).

client() ->
    TCPOptions = [{mode, binary}    % donnée au format bitstring attendu en réception/émission
                 ,{active, false}], % désactivation du mode actif du socket

    Site = "erlang-punch.eu",
    Port = 80,
    io:format("connection vers le site ~p:~p~n", [Site, Port]),
    {ok, Controle} = gen_tcp:connect(Site, Port, TCPOptions),

    Message = <<"GET / HTTP/1.0\r\n\r\n">>,
    io:format("envois du message ~p~n", [Message]),
    gen_tcp:send(Controle, Message),

    {ok, Reponse} = gen_tcp:recv(Controle, 0),
    io:format("réception de la réponse: ~p~n", [Reponse]),

    io:format("fermeture du socket~n", []),
    gen_tcp:close(Controle). 
