%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_SUITE).
-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([cache/1, cache_sup/1, cache_app/1]).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
all() ->
    [cache, cache_sup, cache_app].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
suite() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_per_suite(_Config) ->
    ok = application:ensure_started(cache),
    _Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
cache(Config) ->
    Cache = cache,
    ok = cache:add(Cache, cle, valeur),
    [cle] = cache:get_keys(Cache),
    [valeur] = cache:get_values(Cache),
    valeur = cache:get(Cache, cle),

    ok = cache:delete(Cache, cle),
    [] = cache:get_keys(Cache),
    [] = cache:get_values(Cache),
    undefined = cache:get(Cache, cle).
    
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
cache_sup(Config) ->
    Count = supervisor:count_children(cache_sup),
    3 = proplists:get_value(specs, Count),
    3 = proplists:get_value(active, Count),
    2 = proplists:get_value(supervisors, Count),
    1 = proplists:get_value(workers, Count).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
cache_app(_Config) ->
    CacheSup = erlang:whereis(cache_sup),
    true = erlang:is_pid(CacheSup),
    cache_sup([{pid, CacheSup}]),

    Cache = erlang:whereis(cache),
    true = erlang:is_pid(Cache),
    cache([{pid, Cache}]).
