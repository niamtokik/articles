%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tcp_acceptor).
-export([init/1, terminate/3, callback_mode/0]).
-export([accept/3, wait/3]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").
-record(data, { listener_sock = undefined
              , acceptor_sock = undefined 
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
    gen_tcp:close(AcceptSock).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
wait(internal, accept_socket, #data{ listener_sock = ListenerSock
                                   , acceptor_sock = undefined } = Data) ->
    ?LOG_DEBUG("acceptor ~p wait for connection", [self()]),
    case gen_tcp:accept(ListenerSock) of
        {ok, AcceptorSock} ->
            ?LOG_DEBUG("acceptor ~p received sock ~p", [self(), AcceptorSock]),
            {next_state, accept, Data#data{ acceptor_sock = AcceptorSock } };
        {error, Raison} ->
            {stop, Raison}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
accept(info, {tcp, _Port, Message} = Content, #data{ acceptor_sock = AcceptSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    case cache_lib:parse(Message) of
        {error, Raison} -> gen_tcp:send(AcceptSock, Raison);
        {Module, Command, Args} -> reponse(Module, Command, Args, Data)
    end,
    ok = gen_tcp:close(AcceptSock),
    {next_state, wait, Data#data{ acceptor_sock = undefined }, [{next_event, internal, accept_socket}] };

%
accept(info, {tcp_closed, AcceptSock}, #data{ acceptor_sock = AcceptSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p s'arrête", [self()]),
    ok = gen_tcp:close(AcceptSock),
    {next_state, wait, Data#data{ acceptor_sock = undefined }, [{next_event, internal, accept_socket}] };

%
accept(MessageType, Message, Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p type ~p", [self(), Message, MessageType]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
reponse(Module, Command, Args, #data{ acceptor_sock = AcceptSock }) ->
    Ret = erlang:apply(Module, Command, Args),
    ?LOG_DEBUG("retour de la commande ~p, ~p: ~p", [Command, Args, Ret]),
    case Ret of
        Ret when is_bitstring(Ret) -> 
            gen_tcp:send(AcceptSock, Ret);
        Ret when is_list(Ret) -> 
            gen_tcp:send(AcceptSock, Ret);
        _ -> 
            ?LOG_WARNING("valeur non supportée")
    end.

