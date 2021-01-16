-module(udp_client).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

init(Args) ->
    DestinationAdresse = proplists:get_value(adresse, Args),
    DestinationPort = proplists:get_value(port, Args),
    Message = proplists:get_value(message, Args),
    Timeout = proplists:get_value(timeout, Args, 1000),
    SourcePort = 1024+ceil(rand:uniform()*64511),
    {ok, Controle} = gen_udp:open(SourcePort, [{mode,binary},{active,true}]),
    ok = gen_udp:send(Controle, DestinationAdresse, DestinationPort, Message),
    {ok, _Ref} = timer:apply_after(Timeout, gen_server, cast, [self(), timeout]),
    ?LOG_DEBUG("~p", [Controle]),
    {ok, Controle}.

terminate(Raison, Controle) ->
    ?LOG_DEBUG("terminated reason: ~p", [Raison]),
    gen_udp:close(Controle).

handle_cast(timeout, Controle) ->
    {stop, {shutdown, timeout}, Controle};
handle_cast(Message, Controle) ->
    ?LOG_DEBUG("received cast: ~p", [Message]),
    {noreply, Controle}.

handle_call(Message, From, Controle) ->
    ?LOG_DEBUG("received call from ~p: ~p", [From, Message]),
    {reply, ok, Controle}.

handle_info({udp, Interface, Source, Port, Message} = _Data, Controle) ->
    ?LOG_DEBUG("received ~p from ~p:~p", [Message, Source, Port]),
    {stop, normal, Controle};
handle_info(Message, Controle) ->
    ?LOG_DEBUG("received info: ~p", [Message]),
    {noreply, Controle}.
