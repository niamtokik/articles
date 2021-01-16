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

    {ok, Controle} = gen_tcp:connect("erlang-punch.eu", 80, TCPOptions),

    gen_tcp:send(Controle, <<"GET / HTTP/1.0\r\n\r\n">>),

    {ok, Reponse} = gen_tcp:recv(Controle, 0),

    io:format("~p~n", [Reponse]),

    gen_tcp:close(Controle). 
