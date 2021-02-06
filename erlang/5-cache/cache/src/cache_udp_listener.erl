%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_udp_listener).
-export([start/1, start_link/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc start/1 démarre permet de démarrage automatiquement le serveur
%% UDP au moyen de la fonction gen_server:start/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Arguments) -> Retour when
      Arguments :: integer(),
      Retour :: {ok, pid()}.
start(Arguments) ->
    gen_server:start(?MODULE, Arguments, [debug]).

-spec start_link(Arguments) -> Retour when
      Arguments :: integer(),
      Retour :: {ok, pid()}.
start_link(Arguments) ->
    gen_server:start_link(?MODULE, Arguments, [debug]).

%%--------------------------------------------------------------------
%% @doc init/1 permet d'initialiser le serveur UDP en créant un socket
%% et en le configurant comme état du processus utilisant gen_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(UdpPort) -> Retour when
      UdpPort :: integer(),
      Retour :: {ok, port()}.
init(UdpPort) 
  when is_integer(UdpPort)->
    Options = [{mode, binary}],
    {ok, Interface} = gen_udp:open(UdpPort, Options),
    ?LOG_DEBUG("Écoute sur le port UDP ~p avec l'interface ~p", [UdpPort, Interface]),
    {ok, Interface}.

%%--------------------------------------------------------------------
%% @doc terminate/2 arrête proprement le serveur en fermant le socket
%% UDP du serveur.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Raison, Interface) -> Return when
      Raison :: term(),
      Interface :: port(),
      Return :: ok.
terminate(_Raison, Interface) ->
    gen_udp:close(Interface).

%%--------------------------------------------------------------------
%% @doc handle_cast/2 n'est pas actif pour ce service. Cette fonction
%% ne fait que logger ce qu'elle reçoit.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, Interface) -> Retour when
      Message :: term(),
      Interface :: port(),
      Retour :: {noreply, Interface}.
handle_cast(Message, Interface) ->
    ?LOG_DEBUG("Message cast reçu: ~p", [Message]),
    {noreply, Interface}.

%%--------------------------------------------------------------------
%% @doc handle_call/3 n'est pas actif pour ce service. Cette fonction
%% ne fait que logger ce qu'elle reçoit.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, Interface) -> Retour when
      Message :: term(),
      From :: term(),
      Interface :: port(),
      Retour :: {reply, ok, Interface}.
handle_call(Message, From, Interface) ->
    ?LOG_DEBUG("Message call reçu depuis ~p: ~p", [From, Message]),
    {reply, ok, Interface}.

%%--------------------------------------------------------------------
%% @doc handle_info/2 récupère les messages udp transféré depuis le
%% processus de controle et renvoit automatiquement un message d'echo
%% pour valider son fonctionnement.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Data, Interface) -> Retour when
      Data :: {udp, Process, Source, Port, Message} | term(),
      Process :: port(),
      Source :: tuple(),
      Port :: integer(),
      Message :: bitstring(),
      Interface :: port(),
      Retour :: {noreply, Interface}.
handle_info({udp, Process, Source, Port, Message} = Data, Interface) ->
    ?LOG_DEBUG("Message UDP  reçu depuis ~p:~p: ~p, ~p", [Source, Port, Message, Process]),
    command(Data, Interface),
    {noreply, Interface};
handle_info(Message, Interface) ->
    ?LOG_DEBUG("Message info reçu: ~p", [Message]),
    {noreply, Interface}.

command({udp, Process, Source, Port, Message} = Data, Interface) ->
    Parse = re:split(Message, <<" ">>, [{parts, 3}]),
    case Parse of
        [<<"add">>, Key, Value] -> 
            cache:add(cache, Key, Value);
        [<<"get">>, Key] -> 
            Value = cache:get(cache, Key),
            ?LOG_DEBUG("valeur retournée: ~p", [Value]),
            gen_udp:send(Process, Source, Port, Value);
        [<<"delete">>, Key] -> 
            cache:delete(cache, Key);
        [<<"get_keys">>] -> 
            Keys = cache:get_keys(cache),
            ?LOG_DEBUG("valeur retournée: ~p", [Keys]),
            gen_udp:send(Process, Source, Port, Keys);
        [<<"get_values">>] ->
            Values = cache:get_values(cache),
            ?LOG_DEBUG("valeur retournée: ~p", [Values]),
            gen_udp:send(Process, Source, Port, Values);
        Command -> 
            ?LOG_WARNING("commande ~p non supportée", [Command])
    end.

command_parser(Command) ->
    case Command of
        [<<"add">>, Key, Value] ->
            [add, Key, Value];
        [<<"get">>, Key] ->
            [get, Key];
        [<<"delete">>, Key] -> 
            [delete, Key];
        [<<"get_keys">>] -> 
            [get_keys];
        [<<"get_values">>] ->
            [get_values];
        _ ->
            [bad_command]
    end.
