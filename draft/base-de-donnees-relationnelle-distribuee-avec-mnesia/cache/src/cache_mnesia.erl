-module(cache_mnesia).
-behaviour(gen_server).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3]).

-record(?MODULE, {key, value}).

-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok, map()}.

init(_Args) ->
    init_mnesia(),
    {ok, []}.

init_mnesia() ->
    case [ true || X <- mnesia:system_info(tables), X =:= ?MODULE ] of
        [] ->
            mnesia:create_table(?MODULE, [{attributes, record_info(fields, ?MODULE)}]);
        _ -> ok
    end.

-spec terminate(Raison, Etat) -> Result when
      Raison :: term(),
      Etat :: map(),
      Result :: ok.

terminate(_Raison, _Etat) -> mnesia:delete(?MODULE).

-spec handle_cast(Message, Etat) -> Result when
      Message :: {add, term(), term()} |
                 {delete, term()},
      Etat :: map(),
      Result :: {noreply, map()}.

handle_cast({add, Cle, Valeur}, Etat) ->
    mnesia_write(Cle, Valeur),
    {noreply, Etat};
handle_cast({delete, Cle}, Etat) ->
    mnesia_delete(Cle),
    {noreply, Etat}.

-spec handle_call(Message, From, Etat) -> Resultat when
      Message :: get_keys | get_values | {get, term()},
      From :: {pid(), term()},
      Etat :: map(),
      Resultat :: {reply, term(), Etat}.

handle_call(get_keys, _From, Etat) ->
    Keys = mnesia_keys(),
    {reply, Keys, Etat};
handle_call(get_values, _From, Etat) ->
    Values = mnesia_values(),
    {reply, Values, Etat};
handle_call({get, Cle}, _From, Etat) ->
    case mnesia_get(Cle) of
        [] -> {reply, undefined, Etat};
        [Value] -> {reply, Value, Etat}
    end.

transaction(Transaction) ->
    case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        Elsewise -> error
    end.

mnesia_get(Cle) ->
    transaction(fun() -> mnesia:select(?MODULE, [{#?MODULE{ key = Cle, value = '$1' }, [], ['$1']}]) end).

mnesia_keys() ->
    transaction(fun() -> mnesia:select(?MODULE, [{#?MODULE{ key = '$1', _ = '_' }, [], ['$1']}]) end).

mnesia_values() ->
    transaction(fun() -> mnesia:select(?MODULE, [{#?MODULE{ value = '$1', _ = '_' }, [], ['$1']}]) end).

mnesia_write(Cle, Valeur) ->
    transaction(fun() -> mnesia:write(#?MODULE{ key = Cle, value = Valeur }) end).

mnesia_delete(Cle) ->
    transaction(fun() -> mnesia:delete(?MODULE, Cle) end).
            
