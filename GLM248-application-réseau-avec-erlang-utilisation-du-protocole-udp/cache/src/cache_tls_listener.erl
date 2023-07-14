%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tls_listener).
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
    ?LOG_DEBUG("Démarrage du listener tls ~p: ~p", [self(), Arguments]),
    Port = proplists:get_value(port, Arguments, 8889),
    Acceptors = proplists:get_value(acceptors, Arguments, 10),
    Certificat = proplists:get_value(certificat, Arguments, "cert.pem"),
    Cle = proplists:get_value(cle, Arguments, "key.pem"),
    Options = [{active, true}
              ,{keyfile, Cle}
              ,{certfile, Certificat}
              ,{mode, binary}
              ,{active, true}
              ,{protocol, tls}],
    {ok, Listener} = ssl:listen(Port, Options),
    init_acceptor(cache_tls_acceptor_sup, Listener, Acceptors),
    {ok, active, Listener}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Raison, _State, Listener) ->
    ?LOG_DEBUG("Arrêt du processus ~p", [self()]),
    ssl:close(Listener).

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
    StartArgs = [cache_tls_acceptor, Socket, []],
    Spec = #{ id => {cache_tls_acceptor, erlang:unique_integer()}
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
