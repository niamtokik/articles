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
init(Arguments) ->
    logger:set_module_level(?MODULE, debug),
    Adresse = maps:get(adresse, Arguments),
    Port = maps:get(port, Arguments),
    Message = maps:get(message, Arguments),
    {ok, Client} = gen_tcp:connect(Adresse, Port, [{mode, binary},{active,true}]),
    gen_tcp:send(Client, Message),
    {ok, Client}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, Client) ->
    gen_tcp:close(Client).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({send, Message}, Client) ->
    ?LOG_DEBUG("Donnée TCP à envoyer: ~p", [Message]),
    gen_tcp:send(Client, Message),
    {noreply, Client};
handle_cast(Message, Client) ->
    ?LOG_DEBUG("Message cast cast: ~p", [Message]),
    {noreply, Client}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_call(Message, From, Client) ->
    ?LOG_DEBUG("Message call reçu depuis ~p: ~p", [From, Message]),
    {reply, ok, Client}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_closed, Client}, Client) ->
    {stop, normal, Client};
handle_info(Message, Client) ->
    ?LOG_DEBUG("Message info reçu: ~p", [Message]),
    {noreply, Client}.
