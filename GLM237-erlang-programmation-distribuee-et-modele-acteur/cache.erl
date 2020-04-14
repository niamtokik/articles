%%%-------------------------------------------------------------------
%%% @doc cache  
%%% @end
%%%-------------------------------------------------------------------
-module(cache).
-export([start/0, start/1]).
-export([add/3, update/3, delete/2]).
-export([get/2]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start() -> 
    start([]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start(Args) -> 
    Start = fun() -> init(Args) end,
    erlang:spawn(Start).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    loop(#{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
loop(State) ->
    receive
        {Pid, get, values} ->
            Answer = maps:keys(State),
            erlang:send(Pid, Answer),
            loop(State);
        {Pid, get, keys} ->
            Answer = maps:values(State),
            erlang:send(Pid, Answer),
            loop(State);
        {Pid, get, state} ->
            erlang:send(Pid, State),
            loop(State);
        {add, Key, Value} ->
            NewState = maps:put(Key, Value, State),
            loop(NewState);
        {update, Key, Value} ->
            NewState = maps:update(Key, Value, State),
            loop(NewState);
        {delete, Key}  ->
            NewState = maps:remove(Key, State),
            loop(NewState);
        Else ->
            io:format("got ~p~n", [Else]),
            loop(State)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
add(Pid, Key, Value) ->
    erlang:send(Pid, {add, Key, Value}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
update(Pid, Key, Value) ->
    erlang:send(Pid, {update, Key, Value}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
delete(Pid, Key) ->
    erlang:send(Pid, {delete, Key}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get(Pid, Message) when is_atom(Message) ->
    erlang:send(Pid, {self(), get, Message}),
    receive 
        Data -> {ok, Data}
    after 
        1000 -> {error, timeout}
    end.
