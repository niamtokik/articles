%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_test).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_test() ->
    ?assertEqual({ok, #{}}, 
                 cache:init([])).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
add_key_test() ->
    {ok, Etat} = cache:init([]),
    ?assertEqual({noreply, Etat#{ cle => value}}, 
                 cache:handle_cast({add, cle, value}, Etat)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
delete_key_test() ->
    {ok, Etat} = cache:init([]),
    ?assertEqual({noreply, #{}},
                  cache:handle_cast({delete, cle}, Etat)),

    cache:handle_cast({add, cle, value}, Etat),
    ?assertEqual({noreply, #{}},
                 cache:handle_cast({delete, cle}, Etat)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_keys_test() ->
    {ok, Etat} = cache:init([]),
    ?assertEqual({reply, [], Etat},
                 cache:handle_call(get_keys, undefined, Etat)),

    {noreply, Etat2} = cache:handle_cast({add, cle, value}, Etat),
    ?assertEqual({reply, [cle], Etat2},
                 cache:handle_call(get_keys, undefined, Etat2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_values_test() ->
    {ok, Etat} = cache:init([]),
    ?assertEqual({reply, [], Etat},
                 cache:handle_call(get_values, undefined, Etat)),

    {noreply, Etat2} = cache:handle_cast({add, cle, value}, Etat),
    ?assertEqual({reply, [value], Etat2},
                 cache:handle_call(get_values, undefined, Etat2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_key_test() ->
    {ok, Etat} = cache:init([]),
    ?assertEqual({reply, undefined, Etat},
                 cache:handle_call({get, cle}, undefined, Etat)),

    {noreply, Etat2} = cache:handle_cast({add, cle, value}, Etat),
    ?assertEqual({reply, value, Etat2},
                 cache:handle_call({get, cle}, undefined, Etat2)).

