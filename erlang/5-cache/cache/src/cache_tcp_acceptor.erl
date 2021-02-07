%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tcp_acceptor).
-export([start_link/0, start_link/1]).
-export([init/1, terminate/3, callback_mode/0]).
-export([accept/3]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").
-record(data, { listener_sock = undefined
              , acceptor_sock = undefined 
              }).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link(Arguments) ->
    gen_statem:start_link(?MODULE, Arguments, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
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
    {ok, accept, Data, [{next_event, internal, accept_socket}]}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Raison, _Etat, #data{ acceptor_sock = AcceptSock }) ->
    gen_tcp:close(AcceptSock).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
accept(internal, accept_socket, #data{ listener_sock = ListenerSock
                                     , acceptor_sock = undefined } = Data) ->
    ?LOG_DEBUG("acceptor ~p wait for connection", [self()]),
    {ok, AcceptorSock} = gen_tcp:accept(ListenerSock),
    ?LOG_DEBUG("acceptor ~p received sock ~p", [self(), AcceptorSock]),
    {keep_state, Data#data{ acceptor_sock = AcceptorSock } };

accept(info, {tcp, _Port, Message}, #data{ acceptor_sock = AcceptSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    {keep_state, Data};

accept(info, {tcp_closed, AcceptSock}, #data{ acceptor_sock = AcceptSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p s'arrête", [self()]),
    ok = gen_tcp:close(AcceptSock),
    {next_state, accept, Data#data{ acceptor_sock = undefined }, [{next_event, internal, accept_socket}] };

accept(info, Message, Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    {keep_state, Data}.
