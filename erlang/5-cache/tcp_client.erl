%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_client).
-export([start/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
start(Args) ->
    gen_server:start(?MODULE, Args, [debug]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    logger:set_module_level(?MODULE, debug),
    Adresse = maps:get(adresse, Args),
    Port = maps:get(port, Args),
    Message = maps:get(message, Args),
    {ok, Controle} = gen_tcp:connect(Adresse, Port, [{mode, binary},{active,true}]),
    gen_tcp:send(Controle, Message),
    {ok, Controle}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, Controle) ->
    gen_tcp:close(Controle).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({send, Message}, Controle) ->
    ?LOG_DEBUG("Donnée TCP à envoyer: ~p", [Message]),
    gen_tcp:send(Controle, Message),
    {noreply, Controle};
handle_cast(Message, Controle) ->
    ?LOG_DEBUG("Message cast cast: ~p", [Message]),
    {noreply, Controle}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_call(Message, From, Process) ->
    ?LOG_DEBUG("Message call reçu depuis ~p: ~p", [From, Message]),
    {reply, ok, Process}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_closed, Controle}, Controle) ->
    {stop, normal, Controle};
handle_info(Message, Controle) ->
    ?LOG_DEBUG("Message info reçu: ~p", [Message]),
    {noreply, Controle}.
