%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(exemple_server).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).

%%--------------------------------------------------------------------
%% @doc start/1
%% @end
%%--------------------------------------------------------------------
start() ->
    start([]).

%%--------------------------------------------------------------------
%% @doc start/2
%% @end
%%--------------------------------------------------------------------
start(Arguments) ->
    start(Arguments, []).

%%--------------------------------------------------------------------
%% @doc start/3
%% @end
%%--------------------------------------------------------------------
start(Arguments, Options) ->
    gen_server:start(?MODULE, Arguments, Options).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1
%% @end
%%--------------------------------------------------------------------
start_link(Arguments) ->
    start_link(Arguments, []).

%%--------------------------------------------------------------------
%% @doc start_link/2
%% @end
%%--------------------------------------------------------------------
start_link(Arguments, Options) ->
    gen_server:start_link(?MODULE, Arguments, Options).

%%--------------------------------------------------------------------
%% @doc init/1
%% @end
%%--------------------------------------------------------------------
init(Arguments) ->
    io:format("Argument: ~p~n", [Arguments]),
    {ok, Arguments}.

%%--------------------------------------------------------------------
%% @doc terminate/2
%% @end
%%--------------------------------------------------------------------
terminate(Raison, Etat) ->
    io:format("arret raison: ~p~n", [Raison]),
    io:format("arret etat: ~p~n", [Etat]),
    ok.

%%--------------------------------------------------------------------
%% @doc handle_cast/2
%% @end
%%--------------------------------------------------------------------
handle_cast(Message, Etat) ->
    io:format("cast message: ~p~n", [Message]),
    io:format("cast etat: ~p~n", [Etat]),
    {noreply, Etat}.

%%--------------------------------------------------------------------
%% @doc handle_cast/3
%% @end
%%--------------------------------------------------------------------
handle_call(Message, From, Etat) ->
    io:format("call message: ~p~n", [Message]),
    io:format("call from: ~p~n", [From]),
    io:format("call etat: ~p~n", [Etat]),
    {reply, {Message, From, Etat}, Etat}.

%%--------------------------------------------------------------------
%% @doc handle_info/2
%% @end
%%--------------------------------------------------------------------
handle_info(Message, Etat) ->
    io:format("info message: ~p~n", [Message]),
    io:format("info etat: ~p~n", [Etat]),
    {noreply, Etat}.
