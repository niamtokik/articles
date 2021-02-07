%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tcp_listener).
-export([start_link/1]).
-export([callback_mode/0, init/1, terminate/3]).
-export([active/3, standby/3]).
-export([activate/1, deactivate/1]).
-behavior(gen_statem).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
start_link(Arguments) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Arguments, []).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
callback_mode() -> [state_enter, state_functions].

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
init(Arguments) ->
    logger:set_module_level(?MODULE, debug),
    Port = proplists:get_value(port, Arguments, 8888),
    Acceptors = proplists:get_value(acceptors, Arguments, 10),
    {ok, Listener} = gen_tcp:listen(Port, [binary, {active, true}]),
    [ cache_tcp_acceptor_sup:start_acceptor(Listener) || _ <- lists:seq(1, Acceptors) ],
    {ok, active, Listener}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Raison, _State, Listener) ->
    gen_tcp:close(Listener).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
active(enter, _, Listener) ->
    ?LOG_DEBUG("start listener"),
    {next_state, active, Listener};
active(MessageType, Message, Listener) ->
    ?LOG_DEBUG("receive ~p", [Message]),
    {keep_state, Listener}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
standby(cast, active, Listener) ->
    ?LOG_DEBUG("standby -> active", []),
    {next_state, active, Listener};
standby(MessageType, Message, Listener) ->
    {keep_state, Listener}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
activate(Pid) ->
    gen_statem:cast(Pid, active).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
deactivate(Pid) ->
    gen_statem:cast(Pid, standby).
