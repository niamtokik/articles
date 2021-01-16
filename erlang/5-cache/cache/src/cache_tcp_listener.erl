%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cache_tcp_listener).
-export([start_link/1]).
-export([callback_mode/0]).
-export([init/1, terminate/3]).
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
    gen_statem:start_link(?MODULE, Arguments, []).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
callback_mode() -> state_functions.     

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
init(Arguments) ->
    Listener = [],
    {ok, active, Listener}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(Raison, State, Listener) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
active(cast, standby, Listener) ->
    ?LOG_DEBUG("active -> standby", []),
    {next_state, standby, Listener};
active(MessageType, Message, Listener) ->
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
