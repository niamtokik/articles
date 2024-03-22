%%%===================================================================
%%%
%%%===================================================================
-module(ets_server).
-behavior(gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([start/1, start_link/1]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start(Args) ->
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Args) ->
     {ok, ets:new(?MODULE, Args)}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(_, ETS) ->
     ets:delete(ETS).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(reset, ETS) ->
    ets:delete(ETS),
    {noreply, ETS};
handle_cast(_, ETS) ->
     {noreply, ETS}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call({lookup, Key}, _, ETS) -> {reply, ets:lookup(ETS, Key), ETS};
handle_call({take, Key}, _, ETS) -> {reply, ets:take(ETS, Key), ETS};
handle_call({member, Key}, _, ETS) -> {reply, ets:member(ETS, Key), ETS};
handle_call({delete, Key}, _, ETS) -> {reply, ets:delete(ETS, Key), ETS};
handle_call(export, _, ETS) -> {reply, ets:tab2list(ETS), ETS};
handle_call({match_object, Match}, _, ETS) -> {reply, ets:match_object(ETS, Match), ETS};
handle_call({match, Match}, _, ETS) -> {reply, ets:match(ETS, Match), ETS};
handle_call({insert, Object}, _, ETS)
  when is_tuple(Object), tuple_size(Object) > 1 ->
    {reply, ets:insert(ETS, Object), ETS};
handle_call({select, Match, Guards, Presentation}, _, ETS) ->
    case ets:test_ms({}, [{Match, Guards, Presentation}]) of
        {ok, _} ->
            {reply, ets:select(ETS, [{Match, Guards, Presentation}]), ETS};
                {error, _} = Error -> {reply, Error, ETS}
    end;
handle_call(_, _, ETS) ->
     {reply, false, ETS}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(_, ETS) ->
     {noreply, ETS}.


