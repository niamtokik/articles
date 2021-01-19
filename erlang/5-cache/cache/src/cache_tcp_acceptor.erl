%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tcp_acceptor).
-export([start_link/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link(Arguments) ->
    gen_server:start_link(?MODULE, Arguments, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(ListenerSock) ->
    ?LOG_DEBUG("start acceptor with ~p", [ListenerSock]),
    gen_tcp:accept(ListenerSock).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(Raison, AcceptSock) ->
    gen_tcp:close(AcceptSock).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(_,AcceptSock) ->
    {noreply, AcceptSock}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call(_,_,AcceptSock) ->
    {noreply, AcceptSock}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info({tcp, _Port, Message} = _Data, AcceptSock) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    {noreply, AcceptSock};
handle_info({tcp_closed, AcceptSock} = _Data, AcceptSock) ->
    ?LOG_DEBUG("Acceptor ~p s'arrête", [self()]),
    {stop, normal, AcceptSock};
handle_info(Message, AcceptSock) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    io:format("received ~p~n", [Message]),
    {noreply, AcceptSock}.

