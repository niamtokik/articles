%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%%
%%% @doc Exemple d'utilisation, avec le shell Erlang (ce code
%%% considère qu'un processus udp_serveur est en écoute sur le port
%%% 31415).
%%%
%%%   1> c(udp_client).
%%%   2> logger:set_module_level(udp_client, debug).
%%%   3> udp_client:start([{adresse, "localhost"}
%%%                       ,{port, 31415}
%%%                       ,{message, <<"test">>}]).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(udp_client).
-export([start/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc start/1 démarre le client UDP en utilisant la fonction
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
%% @doc init/1 paramètre le client UDP en créant un socket et
%% configurant le timeout de la connexion.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Arguments) -> Retour when
      Arguments :: proplists:proplist(),
      Retour :: {ok, port()}.
init(Arguments) ->
    DestinationAdresse = proplists:get_value(adresse, Arguments),
    DestinationPort = proplists:get_value(port, Arguments),
    Message = proplists:get_value(message, Arguments),
    Timeout = proplists:get_value(timeout, Arguments, 1000),
    SourcePort = 1024+ceil(rand:uniform()*64511),
    {ok, Interface} = gen_udp:open(SourcePort, [{mode,binary},{active,true}]),
    ok = gen_udp:send(Interface, DestinationAdresse, DestinationPort, Message),
    {ok, _Ref} = timer:apply_after(Timeout, gen_server, cast, [self(), timeout]),
    ?LOG_DEBUG("Interface de contrôle: ~p", [Interface]),
    {ok, Interface}.

%%--------------------------------------------------------------------
%% @doc terminate/2 arrête proprement le service en fermant le socket.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Raison, Interface) -> Retour when
      Raison :: term(),
      Interface :: port(),
      Retour :: ok.
terminate(Raison, Interface) ->
    ?LOG_DEBUG("Raison de l'arrêt: ~p", [Raison]),
    gen_udp:close(Interface).

%%--------------------------------------------------------------------
%% @doc handle_cast/2 arrête le processus si il reçoit le message
%% "timeout" dans sa boite aux lettres.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, Interface) -> Retour when
      Message :: timeout | term(),
      Interface :: port(),
      Retour :: {noreply, Interface} |
                {stop, {shutdown, timeout}, Interface}.
handle_cast(timeout, Interface) ->
    ?LOG_DEBUG("Message de timeout reçu, arrêt du processus", []),
    {stop, {shutdown, timeout}, Interface};
handle_cast(Message, Interface) ->
    ?LOG_DEBUG("Message cast reçu: ~p", [Message]),
    {noreply, Interface}.

%%--------------------------------------------------------------------
%% @doc handle_call/3 n'est pas actif pour ce module.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, Interface) -> Retour when
      Message :: term(),
      From :: pid(),
      Interface :: port(),
      Retour :: {reply, ok, Interface}.
handle_call(Message, From, Interface) ->
    ?LOG_DEBUG("Message call reçu depuis ~p: ~p", [From, Message]),
    {reply, ok, Interface}.

%%--------------------------------------------------------------------
%% @doc handle_info/2 récupère la réponse du serveur et l'affiche au
%% moyen de la fonction debug présent dans logger.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Data, Interface) -> Retour when
      Data :: {udp, Interface, Source, Port, Message} | term(),
      Interface :: port(),
      Source :: term(),
      Port :: integer(),
      Message :: bitstring(),
      Retour :: {stop, normal, Interface} | {noreply, Interface}.
handle_info({udp, Interface, Source, Port, Message} = _Data, Interface) ->
    ?LOG_DEBUG("Message UDP reçu ~p depuis ~p:~p", [Message, Source, Port]),
    {stop, normal, Interface};
handle_info(Message, Interface) ->
    ?LOG_DEBUG("Message info reçu: ~p", [Message]),
    {noreply, Interface}.
