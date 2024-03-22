%%%-------------------------------------------------------------------
%%% @doc le module cache permet de créer un processus permettant de
%%% stockée des clés associés à des valeurs.
%%% @end
%%%-------------------------------------------------------------------
-module(cache).
-export([add/3, delete/2, get/2, get_keys/1, get_values/1]).
-export([start_link/1]).

%%--------------------------------------------------------------------
%% @doc start_link/0 permet d'utiliser la fonction
%% gen_server:start_link/3 pour démarrer un processus lié.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Mode) -> {ok, Pid} when
      Mode :: map | ets,
      Pid :: {ok, pid()} | {error, term()}.
start_link(Mode) ->
    case Mode of
        map -> gen_server:start_link({local, cache}, cache_map, [], []);
        ets -> gen_server:start_link({local, cache}, cache_ets, [], []);
        mnesia -> gen_server:start_link({local, cache}, cache_mnesia, [], []);
        _ -> {error, unsupported_mode}
    end.

%%--------------------------------------------------------------------
%% @doc add/3 est une interface permettant de rajouter une clé
%% associée à une valeur.
%% @end
%%--------------------------------------------------------------------
-spec add(Pid, Cle, Valeur) -> Resultat when
      Pid :: pid() | atom(),
      Cle :: term(),
      Valeur :: term(),
      Resultat :: ok.
add(Pid, Cle, Valeur) ->
    gen_server:cast(Pid, {add, Cle, Valeur}).

%%--------------------------------------------------------------------
%% @doc delete/2 est une interface permettant de supprimer une clé
%% avec sa valeur associée.
%% @end
%%--------------------------------------------------------------------
-spec delete(Pid, Cle) -> Resultat when
      Pid :: pid() | atom(),
      Cle :: term(),
      Resultat :: ok.
delete(Pid, Cle) ->
    gen_server:cast(Pid, {delete, Cle}).

%%--------------------------------------------------------------------
%% @doc get_keys/1 permet de récupérer la liste des clés.
%% @end
%%--------------------------------------------------------------------
-spec get_keys(Pid) -> Resultat when
      Pid :: pid() | atom(),
      Resultat :: [] | [term()].
get_keys(Pid) ->
    gen_server:call(Pid, get_keys).

%%--------------------------------------------------------------------
%% @doc get_values/1 permet de récupérer la liste des valeurs.
%% @end
%%--------------------------------------------------------------------
-spec get_values(Pid) -> Resultat when
      Pid :: pid() | atom(),
      Resultat :: [] | [term()].
get_values(Pid) ->
    gen_server:call(Pid, get_values).

%%--------------------------------------------------------------------
%% @doc get/2 permet de récupéré une valeur associée à un clé.
%% @end
%%--------------------------------------------------------------------
-spec get(Pid, Cle) -> Resultat when
      Pid :: pid() | atom(),
      Cle :: term(),
      Resultat :: undefined | term().
get(Pid, Cle) ->
    gen_server:call(Pid, {get, Cle}).
