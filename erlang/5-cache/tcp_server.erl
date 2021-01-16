%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%%
%%% @doc Exemple d'utilisation, d'abord avec le shell Erlang:
%%%
%%%    1> c(tcp_server).
%%%    2> logger:set_module_level(tcp_server, debug).
%%%    3> tcp_server:start([]).
%%%
%%%  Sur un shell classique:
%%%
%%%    jot 100 1 \
%%%      | xargs -I%i -P20 \
%%%        sh -c "echo %i | nc -vw1 127.0.0.1 8888"
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server).
-export([start/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc start/1 démarre le serveur TCP en utilisant la fonction
%% gen_server:start/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Arguments) -> Retour when
      Arguments :: proplists:proplist(),
      Retour :: {ok, pid()}.
start(Arguments) ->
    gen_server:start(?MODULE, Arguments, [debug]).

%%--------------------------------------------------------------------
%% @doc init/1 paramètre le processus utilisant le behavior
%% gen_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Arguments) -> Retour when
      Arguments :: proplists:proplist(),
      Retour :: {ok, port()}.
init(Arguments) ->
    logger:set_module_level(?MODULE, debug),
    Port = proplists:get_value(port, Arguments, 8888),
    {ok, Listener} = gen_tcp:listen(Port, [binary, {active, true}]),
    [ spawn_monitor(fun() -> acceptor(Listener) end) || _ <- lists:seq(0,100) ],
    io:format("~p~n", [erlang:process_info(self(), monitors)]),
    {ok, Listener}.

%%--------------------------------------------------------------------
%% @doc terminate/2 arrête proprement les acceptors et ferme le
%% socket.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Raison, Listener) -> Retour when
      Raison :: term(),
      Listener :: port(),
      Retour :: ok.
terminate(_Raison, Listener) ->
    {monitors, List} = erlang:process_info(self(), monitors),
    [ erlang:exit(Pid,kill) || {process, Pid} <- List ],
    gen_tcp:close(Listener).

%%--------------------------------------------------------------------
%% @doc acceptor/1 initialise le processus utilisant la fonction
%% acceptor_loop/1 permettant d'accepter les connexions clientes.
%%
%% @end
%%--------------------------------------------------------------------
-spec acceptor(Listener) -> Retour when
      Listener :: port(),
      Retour :: any().
acceptor(Listener) ->
    ?LOG_DEBUG("Démarre un nouveau acceptor pour ~p", [Listener]),
    {ok, AcceptSock} = gen_tcp:accept(Listener),
    acceptor_loop(AcceptSock).

%%--------------------------------------------------------------------
%% @doc acceptor_loop/1 est la fonction acceptant les connexions
%% clientes, affichant sur l'interface de debug les informations
%% reçue. Elle permet, entre autre, de réagir aux différents messages
%% standards de TCP.
%%
%% @end
%%--------------------------------------------------------------------
-spec acceptor_loop(AcceptSock) -> Retour when
      AcceptSock :: port(),
      Retour :: ok.
acceptor_loop(AcceptSock) ->
    receive
        {tcp, _Port, Message} = _Data ->
            ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
            acceptor_loop(AcceptSock);
        {tcp_closed, AcceptSock} = _Data ->
            ?LOG_DEBUG("Acceptor ~p s'arrête", [self()]),
            gen_tcp:close(AcceptSock);
        Message ->
            ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
            io:format("received ~p~n", [Message]),
            acceptor_loop(AcceptSock)
    end.

%%--------------------------------------------------------------------
%% @doc handle_cast/2 n'est pas utilisé dans ce module.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, Listener) -> Retour when
      Message :: term(),
      Listener :: port(),
      Retour :: {noreply, Listener}.
handle_cast(Message, Listener) ->
    ?LOG_DEBUG("Message cast reçu: ~p", [Message]),
    {noreply, Listener}.

%%--------------------------------------------------------------------
%% @doc handle_call/3 n'est pas utilisé dans ce module.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, Listener) -> Retour when
      Message :: term(),
      From :: pid(),
      Listener :: port(),
      Retour :: {reply, ok, Listener}.
handle_call(Message, From, Listener) ->
    ?LOG_DEBUG("Message call reçu ~p depuis ~p", [From, Message]),
    {reply, ok, Listener}.

%%--------------------------------------------------------------------
%% @doc handle_info/2 récupère les messages des processus acceptors
%% qui se terminent, puis, en redémarre un automatiquement.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, Listener) -> Retour when
      Message :: {'DOWN', reference(), process, pid(), term()} | term(),
      Listener :: port(),
      Retour :: {noreply, Listener}.
handle_info({'DOWN', _Ref, process, Process, _}, Listener) ->
    ?LOG_DEBUG("Arrêt de l'acceptor ~p", [Process]),
    spawn_monitor(fun() -> acceptor(Listener) end),
    {noreply, Listener};
handle_info(Message, Listener) ->
    ?LOG_DEBUG("Message info reçu: ~p", [Message]),
    {noreply, Listener}.
