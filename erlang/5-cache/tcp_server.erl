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
-export([start/1, start_link/1]).
-export([init/1, terminate/3, callback_mode/0]).
-export([handle_event/4]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc start/1 démarre le serveur TCP en utilisant la fonction
%% gen_statem:start/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Arguments) -> Retour when
      Arguments :: proplists:proplist(),
      Retour :: {ok, pid()}.
start(Arguments) ->
    gen_statem:start_link(?MODULE, Arguments, [debug]).

%%--------------------------------------------------------------------
%% @doc start_link/1 démarre le serveur TCP en utilisant la fonction
%% gen_statem:start_link/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Arguments) -> Retour when
      Arguments :: proplists:proplist(),
      Retour :: {ok, pid()}.
start_link(Arguments) ->
    gen_statem:start_link(?MODULE, Arguments, [debug]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec callback_mode() -> Return when
      Return :: atom().
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @doc init/1 paramètre le processus utilisant le behavior
%% gen_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Arguments) -> Retour when
      Arguments :: proplists:proplist(),
      Retour :: {ok, State, port()},
      State :: atom().
init(Arguments) ->
    ?LOG_DEBUG("Démarrage du module ~p, ~p: ~p", [?MODULE, self(), Arguments]),
    Port = proplists:get_value(port, Arguments, 8888),
    Acceptors = proplists:get_value(acceptors, Arguments, 100),
    {ok, Listener} = gen_tcp:listen(Port, [binary, {active, true}]),
    [ spawn_monitor(fun() -> acceptor(Listener) end) || _ <- lists:seq(0,Acceptors) ],
    {ok, started, Listener}.

%%--------------------------------------------------------------------
%% @doc terminate/2 arrête proprement les acceptors et ferme le
%% socket.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Raison, Etat, Listener) -> Retour when
      Raison :: term(),
      Etat :: atom(),
      Listener :: port(),
      Retour :: ok.
terminate(_Raison, _Etat, Listener) ->
    ?LOG_DEBUG("Arrêt du processus ~p en écoute", [self()]),
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
    ?LOG_DEBUG("~p attend la réception d'un message", [self()]),
    receive
        {tcp, Client, Message} = _Data ->
            ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
            gen_tcp:send(Client, <<"echo: ", Message/bitstring>>),
            acceptor_loop(AcceptSock);
        {tcp_closed, AcceptSock} = _Data ->
            ?LOG_DEBUG("Acceptor ~p s'arrête", [self()]),
            gen_tcp:close(AcceptSock);
        Message ->
            ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
            acceptor_loop(AcceptSock)
    end.

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Type, Message, State, Listener) -> Retour when
      Type :: info,
      Message :: {'DOWN', reference(), process, pid(), term()} | term(),
      State :: atom(),
      Listener :: port(),
      Retour :: {noreply, Listener}.

handle_event({call, From}, Message, started, Listener) ->
    ?LOG_DEBUG("Message call reçu ~p depuis ~p", [Message, From]),
    {keep_state, Listener, [{reply, From, ok}]};

handle_event(cast, Message, started, Listener) ->
    ?LOG_DEBUG("Message cast reçu: ~p", [Message]),
    {keep_state, Listener};

handle_event(info, {'DOWN', _Ref, process, Process, _}, started, Listener) ->
    ?LOG_DEBUG("Arrêt de l'acceptor ~p", [Process]),
    spawn_monitor(fun() -> acceptor(Listener) end),
    {keep_state, Listener};

handle_event(Type, Message, started, Listener) ->
    ?LOG_DEBUG("Réception d'un message ~p contenant ~p", [Type, Message]),
    {keep_state, Listener}.

    

