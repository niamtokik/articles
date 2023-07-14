%%%-------------------------------------------------------------------
%%% @doc le module cache permet de créer un processus permettant de
%%% stockée des clés associés à des valeurs.
%%% @end
%%%-------------------------------------------------------------------
-module(cache).
-behaviour(gen_server).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3]).
-export([add/3, delete/2, get/2, get_keys/1, get_values/1]).
-export([start_link/0]).

%%--------------------------------------------------------------------
%% @doc start_link/0 permet d'utiliser la fonction
%% gen_server:start_link/3 pour démarrer un processus lié.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid} when
      Pid :: pid().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc ce processus ajout un event handler au démarrage et configure
%% l'état avec une map nulle.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok, map()}.
init(_Args) ->
    Etat = #{},
    {ok, Etat}.

%%--------------------------------------------------------------------
%% @doc terminate/2 ne fait rien et n'est pas essentielle dans ce cas
%% là.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Raison, Etat) -> Result when
      Raison :: term(),
      Etat :: map(),
      Result :: ok.
terminate(_Raison, _Etat) ->
  ok.

%%--------------------------------------------------------------------
%% @doc handle_cast/2 est un callback permet, dans ce cas là de:
%%   - créé une nouvelle clé/valeur
%%   - supprimé une clé et sa valeur associée
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, Etat) -> Result when
      Message :: {add, term(), term()} |
                 {delete, term()},
      Etat :: map(),
      Result :: {noreply, map()}.
handle_cast({add, Cle, Valeur}, Etat) ->    
    {noreply, maps:put(Cle, Valeur, Etat)};
handle_cast({delete, Cle}, Etat) ->
    {noreply, maps:remove(Cle, Etat)}.

%%--------------------------------------------------------------------
%% @doc handle_cast/ est un callback qui permet de:
%%   - récupérer la liste des clés
%%   - récupérer la liste des valeurs
%%   - récupéré la valeur associée à une clé.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, Etat) -> Resultat when
      Message :: get_keys | get_values | {get, term()},
      From :: {pid(), term()},
      Etat :: map(),
      Resultat :: {reply, term(), Etat}.
handle_call(get_keys, _From, Etat) ->
    {reply, maps:keys(Etat), Etat};
handle_call(get_values, _From, Etat) ->
    {reply, maps:values(Etat), Etat};
handle_call({get, Cle}, _From, Etat) ->
    {reply, maps:get(Cle, Etat, undefined), Etat}.

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
