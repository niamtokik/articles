%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server2).
-export([start/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

start(Args) ->
    gen_server:start(?MODULE, Args, [debug]).

init(Args) ->
    logger:set_module_level(?MODULE, debug),
    Port = proplists:get_value(port, Args, 8888),
    {ok, Listener} = gen_tcp:listen(Port, [binary, {active, true}]),
    [ spawn_monitor(fun() -> acceptor(Listener) end) || _ <- lists:seq(0,100) ],
    io:format("~p~n", [erlang:process_info(self(), monitors)]),
    {ok, Listener}.

terminate(_Reason, Process) ->
    {monitors, List} = erlang:process_info(self(), monitors),
    [ erlang:exit(Pid,kill) || {process, Pid} <- List ],
    gen_tcp:close(Process).

acceptor(Listener) ->
    {ok, AcceptSock} = gen_tcp:accept(Listener),
    acceptor_loop(AcceptSock).

acceptor_loop(AcceptSock) ->
    receive
        {tcp, Port, Message} = Data ->
            cache_lib:command(Data),
            acceptor_loop(AcceptSock);
        {tcp_closed, AcceptSock} ->
            gen_tcp:close(AcceptSock);
        Message ->
            io:format("received ~p~n", [Message]),
            acceptor_loop(AcceptSock)
    end.

handle_cast(Message, Process) ->
    ?LOG_DEBUG("received cast: ~p", [Message]),
    {noreply, Process}.

handle_call(Message, From, Process) ->
    ?LOG_DEBUG("received call from ~p: ~p", [From, Message]),
    {reply, ok, Process}.

handle_info({'DOWN', _Ref, process, Process, _}, Listener) ->
    spawn_monitor(fun() -> acceptor(Listener) end),
    {noreply, Listener};
handle_info(Message, Listener) ->
    ?LOG_DEBUG("received info: ~p", [Message]),
    {noreply, Listener}.
