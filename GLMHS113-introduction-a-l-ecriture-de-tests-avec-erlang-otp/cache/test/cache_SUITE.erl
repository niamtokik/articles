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
init_per_testcase(cache, Config) ->
    {ok, Pid} = cache:start_link(),
    [{pid,Pid}|Config];
init_per_testcase(cache_sup, Config) ->
    {ok, Pid} = cache_sup:start_link(),
    [{pid,Pid}|Config];
init_per_testcase(cache_app, Config) ->
    ok = application:start(cache),
    Config;
init_per_testcase(crash_test, Config) ->
    ok = application:start(cache),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
end_per_testcase(cache, Config) ->
    Pid = proplists:get_value(pid, Config),
    gen_server:stop(Pid);
end_per_testcase(cache_sup, Config) ->
    Pid = proplists:get_value(pid, Config),
    gen_server:stop(Pid);
end_per_testcase(cache_app, _Config) ->
    application:stop(cache);
end_per_testcase(crash_test, _Config) ->
    application:stop(cache).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
cache(Config) ->
    Cache = proplists:get_value(pid, Config),
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
    CacheSup = proplists:get_value(pid, Config),
    Count = supervisor:count_children(CacheSup),
    1 = proplists:get_value(specs, Count),
    1 = proplists:get_value(active, Count),
    0 = proplists:get_value(supervisors, Count),
    1 = proplists:get_value(workers, Count),
    [{cache, Cache, worker, [gen_server]}] = supervisor:which_children(CacheSup),
    cache([{pid, Cache}]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
cache_app(_Config) ->
    ok = application:ensure_started(cache),
    CacheSup = erlang:whereis(cache_sup),
    true = erlang:is_pid(CacheSup),
    cache_sup([{pid, CacheSup}]),

    Cache = erlang:whereis(cache),
    true = erlang:is_pid(Cache),
    cache([{pid, Cache}]).

