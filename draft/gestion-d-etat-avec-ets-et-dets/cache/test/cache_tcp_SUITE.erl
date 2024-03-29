%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_tcp_SUITE).
-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([tcp/1]).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
all() -> [tcp].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
suite() -> [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:ensure_started(cache),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_, Config) -> Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_, Config) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
tcp(Config) ->
    Cle = <<"key">>,
    Valeur = <<"Valeur">>,
    ok = cache_tcp_client:add(localhost, 8888, Cle, Valeur),
    {ok, Valeur} = cache_tcp_client:get(localhost, 8888, Cle),
    {ok, Cle} = cache_tcp_client:get_keys(localhost, 8888),
    {ok, Valeur} = cache_tcp_client:get_values(localhost, 8888),
    ok = cache_tcp_client:delete(localhost, 8888, Cle).

