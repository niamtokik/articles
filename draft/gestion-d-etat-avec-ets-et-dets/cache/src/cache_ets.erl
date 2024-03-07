-module(cache_ets).
-behaviour(gen_server).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3]).

-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok, map()}.

init(_Args) ->
    Store = ets:new(?MODULE, []),
    {ok, Store}.

-spec terminate(Raison, Etat) -> Result when
      Raison :: term(),
      Etat :: map(),
      Result :: ok.

terminate(_Raison, _Etat) ->
  ok.

-spec handle_cast(Message, Etat) -> Result when
      Message :: {add, term(), term()} |
                 {delete, term()},
      Etat :: map(),
      Result :: {noreply, map()}.

handle_cast({add, Cle, Valeur}, Etat) ->    
    ets:insert(Etat, {Cle, Valeur}),
    {noreply, Etat};
handle_cast({delete, Cle}, Etat) ->
    ets:delete(Etat, Cle),
    {noreply, Etat}.

-spec handle_call(Message, From, Etat) -> Resultat when
      Message :: get_keys | get_values | {get, term()},
      From :: {pid(), term()},
      Etat :: map(),
      Resultat :: {reply, term(), Etat}.

handle_call(get_keys, _From, Etat) ->
    {reply, ets:match(Etat, {'$1', '_'}), Etat};
handle_call(get_values, _From, Etat) ->
    {reply, ets:match(Etat, {'_', '$1'}), Etat};
handle_call({get, Cle}, _From, Etat) ->
    case ets:select(Etat, [{{Cle, '$2'}, [], ['$2']}]) of
        [] -> {reply, undefined, Etat};
        [Value] -> {reply, Value, Etat}
    end.
