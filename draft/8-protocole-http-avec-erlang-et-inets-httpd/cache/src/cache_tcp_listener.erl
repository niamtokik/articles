%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tcp_listener).
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
callback_mode() -> [state_enter, handle_event_function].

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
init(Arguments) ->
    ?LOG_DEBUG("Démarrage du listener ~p: ~p", [self(), Arguments]),
    Port = proplists:get_value(port, Arguments, 8888),
    Acceptors = proplists:get_value(acceptors, Arguments, 10),
    {ok, Listener} = gen_tcp:listen(Port, [binary, {active, true}]),
    init_acceptor(cache_tcp_acceptor_sup, Listener, Acceptors),
    {ok, active, Listener}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Raison, _State, Listener) ->
    ?LOG_DEBUG("Arrêt du processus ~p", [self()]),
    gen_tcp:close(Listener).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_event(enter, _, active, Listener) ->   
    ?LOG_DEBUG("État actif du processus ~p", [self()]),
    {next_state, active, Listener}; 
handle_event(MessageType, Message, active, Listener) ->
    ?LOG_DEBUG("Message ~p de type ~p reçu", [Message, MessageType]),
    {keep_state, Listener}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
start_acceptor(Supervisor, Socket) ->
    StartArgs = [cache_tcp_acceptor, Socket, []],
    Spec = #{ id => {cache_tcp_acceptor, erlang:unique_integer()}
            , start => { gen_statem, start_link, StartArgs }
            , restart => permanent
            , type => worker
            },
    supervisor:start_child(Supervisor, Spec).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
init_acceptor(Supervisor, Socket, Acceptors) ->
    [ start_acceptor(Supervisor, Socket) || _ <- lists:seq(0, Acceptors) ].
