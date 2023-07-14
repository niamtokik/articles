%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tls_acceptor).
-export([init/1, terminate/3, callback_mode/0]).
-export([accept/3, wait/3]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").
-record(data, { listener_sock = undefined
              , acceptor_sock = undefined 
              , handshake_sock = undefined
              }).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
%%--------------------------------------------------------------------
init(ListenerSock) ->
    logger:set_module_level(?MODULE, debug),
    ?LOG_DEBUG("start acceptor ~p with ~p", [self(), ListenerSock]),
    Data = #data{ listener_sock = ListenerSock },
    {ok, wait, Data, [{next_event, internal, accept_socket}]}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%-------------------------------------------------------------------
terminate(_Raison, _Etat, #data{ acceptor_sock = undefined }) ->
    ok;
terminate(_Raison, _Etat, #data{ acceptor_sock = AcceptSock }) ->
    ssl:close(AcceptSock).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
wait(internal, accept_socket, #data{ listener_sock = ListenerSock
                                   , acceptor_sock = undefined 
                                   , handshake_sock = undefined } = Data) ->
    ?LOG_DEBUG("acceptor ~p wait for connection", [self()]),
    case ssl:transport_accept(ListenerSock) of
        {ok, AcceptorSock} ->
            ?LOG_DEBUG("acceptor ~p received sock ~p", [self(), AcceptorSock]),
            {next_state
            ,wait
            ,Data#data{ acceptor_sock = AcceptorSock }
            ,[{next_event, internal,  handshake_sock}] 
            };
        {error, Raison} ->
            {stop, Raison}
    end;
wait(internal, handshake_sock, #data{ listener_sock = _ListenerSock
                                    , acceptor_sock = AcceptorSock
                                    , handshake_sock = undefined } = Data) ->
    ?LOG_DEBUG("handshake ~p", [self()]),
    case ssl:handshake(AcceptorSock) of
        {ok, HandshakeSock} ->
            {next_state
            ,accept
            ,Data#data{ handshake_sock = HandshakeSock }
            };
        {error, Raison} ->
            {stop, Raison}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
accept(info, {ssl, _Port, Message} = Content, #data{ handshake_sock = HandshakeSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    case cache_lib:parse(Message) of
        {error, Raison} -> 
            ssl:send(HandshakeSock, Raison);
        {Module, Command, Args} -> 
            reponse(Module, Command, Args, Data)
    end,
    ok = ssl:close(HandshakeSock),
    {next_state, wait, Data#data{ acceptor_sock = undefined, handshake_sock = undefined }, [{next_event, internal, accept_socket}] };

%
accept(info, {tcp_closed, AcceptSock}, #data{ handshake_sock = HandshakeSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p s'arrête", [self()]),
    ok = ssl:close(HandshakeSock),
    {next_state, wait, Data#data{ acceptor_sock = undefined }, [{next_event, internal, accept_socket}] };

%
accept(MessageType, Message, Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p type ~p", [self(), Message, MessageType]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
reponse(Module, Command, Args, #data{ handshake_sock = HandshakeSock }) ->
    Ret = erlang:apply(Module, Command, Args),
    ?LOG_DEBUG("retour de la commande ~p, ~p: ~p", [Command, Args, Ret]),
    case Ret of
        ok -> ok;
        Ret when is_bitstring(Ret) -> 
            ssl:send(HandshakeSock, Ret);
        Ret when is_list(Ret) -> 
            ssl:send(HandshakeSock, Ret);
        _ -> 
            ?LOG_WARNING("valeur non supportée")
    end.

