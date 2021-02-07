%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tcp_acceptor).
-export([start_link/0, start_link/1]).
-export([init/1, terminate/3, callback_mode/0]).
-export([accept/3, wait/3]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").
-record(data, { listener_sock = undefined
              , acceptor_sock = undefined 
              }).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link(Arguments) ->
    gen_statem:start_link(?MODULE, Arguments, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
%%--------------------------------------------------------------------
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
%%--------------------------------------------------------------------
init(ListenerSock) ->
    logger:set_module_level(?MODULE, debug),
    ?LOG_DEBUG("start acceptor ~p with ~p", [self(), ListenerSock]),
    Data = #data{ listener_sock = ListenerSock },
    {ok, wait, Data, [{next_event, internal, accept_socket}]}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%-------------------------------------------------------------------
terminate(_Raison, _Etat, #data{ acceptor_sock = undefined }) ->
    ok;
terminate(_Raison, _Etat, #data{ acceptor_sock = AcceptSock }) ->
    gen_tcp:close(AcceptSock).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
wait(internal, accept_socket, #data{ listener_sock = ListenerSock
                                   , acceptor_sock = undefined } = Data) ->
    ?LOG_DEBUG("acceptor ~p wait for connection", [self()]),
    case gen_tcp:accept(ListenerSock) of
        {ok, AcceptorSock} ->
            ?LOG_DEBUG("acceptor ~p received sock ~p", [self(), AcceptorSock]),
            {next_state, accept, Data#data{ acceptor_sock = AcceptorSock } };
        {error, Raison} ->
            {stop, Raison}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
accept(info, {tcp, _Port, Message} = Content, #data{ acceptor_sock = AcceptSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    command(Content, Data),
    {keep_state, Data};

accept(info, {tcp_closed, AcceptSock}, #data{ acceptor_sock = AcceptSock } = Data) ->
    ?LOG_DEBUG("Acceptor ~p s'arrête", [self()]),
    ok = gen_tcp:close(AcceptSock),
    {next_state, wait, Data#data{ acceptor_sock = undefined }, [{next_event, internal, accept_socket}] };

accept(info, Message, Data) ->
    ?LOG_DEBUG("Acceptor ~p reçoit ~p", [self(), Message]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
command({tcp, Process, Message} = Content, Data) ->
    Parse = re:split(Message, <<" ">>, [{parts, 3}]),
    case Parse of
        [<<"add">>, Key, Value] -> 
            cache:add(cache, Key, Value);
        [<<"get">>, Key] -> 
            Value = cache:get(cache, Key),
            ?LOG_DEBUG("valeur retournée: ~p", [Value]),
            gen_tcp:send(Process, Value);
        [<<"delete">>, Key] -> 
            cache:delete(cache, Key);
        [<<"get_keys">>] -> 
            Keys = cache:get_keys(cache),
            ?LOG_DEBUG("valeur retournée: ~p", [Keys]),
            gen_tcp:send(Process, Keys);
        [<<"get_values">>] ->
            Values = cache:get_values(cache),
            ?LOG_DEBUG("valeur retournée: ~p", [Values]),
            gen_tcp:send(Process, Values);
        Command -> 
            ?LOG_WARNING("commande ~p non supportée", [Command])
    end.
