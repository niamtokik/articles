%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_cli).
-export([main/1]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
main(Args) -> main(Args, #{});
main(["help"|_]) -> main([]);
main(["server"]) -> execute(#{ args => "cache server [start|help]" });
main(["server","help"|_]) -> main(["server"]);
main(["client"]) -> execute(#{ args => "cache client [http|udp|tcp|tls|ssh|help]" });
main(["client","help"|_]) -> execute(#{ args => "cache client [http|udp|tcp|tls|ssh|help]" });
main(["client","http"]) -> main(["client","help"]);
main(["client","http","add"|Rest]) -> execute(#{ function => add, args => [http,Rest] });
main(["client","http","get"|Rest]) -> execute(#{ function => get, args => [http,Rest] });
main(["client","http","delete"|Rest]) -> execute(#{ function => delete, args => [http,Rest] }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
main([], #{}) -> execute(#{ args => ["cache [server|client|help]"] });
main([], _) -> execute(#{ args => ["cache [server|client|help]"] }).

env() -> 
    Filter = fun("CACHE_"++ _) -> true; (_) -> false end,
    lists:filter(Filter, os:getenv()).

env(Key) ->
    Target = "CACHE_" ++ Key,
    Filter = fun("CACHE_" ++ Key) -> true; (_) -> false end,
    case lists:filter(Filter, env()) of
        "CACHE_" ++ [$=, Value] -> {ok, Value};
        _ -> {error, undefined}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
execute(#{ module := M, function := F, args := A }) -> erlang:apply(M, F, A);
execute(#{ function := F, args := A }) -> erlang:apply(?MODULE, F, A);
execute(#{ args := A }) -> erlang:apply(?MODULE, usage, A).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
usage(Message) -> io:format("Usage: ~s~n", [Message]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
add(_Method, ["-key", _Key, "-value", _Value]) -> ok.    

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
get(_Method, ["-key", _Key]) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
delete(_Method, ["-key", _Key]) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
list_keys(_Method) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
list_values(_Method) -> ok.

