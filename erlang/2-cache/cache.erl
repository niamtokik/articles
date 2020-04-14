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
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc ce processus ajout un event handler au démarrage et configure
%% l'état avec une map nulle.
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    Etat = #{},
    {ok, Etat}.

%%--------------------------------------------------------------------
%% @doc terminate/2 ne fait rien et n'est pas essentielle dans ce cas
%% là.
%% @end
%%--------------------------------------------------------------------
terminate(_Raison, _Etat) ->
  ok.

%%--------------------------------------------------------------------
%% @doc handle_cast/2 est un callback permet, dans ce cas là de:
%%   - créé une nouvelle clé/valeur
%%   - supprimé une clé et sa valeur associée
%% @end
%%--------------------------------------------------------------------
handle_cast({add, Key, Value}, Etat) ->    
    {noreply, maps:put(Key, Value, Etat)};
handle_cast({delete, Key}, Etat) ->
    {noreply, maps:remove(Key, Etat)}.

%%--------------------------------------------------------------------
%% @doc handle_cast/ est un callback qui permet de:
%%   - récupérer la liste des clés
%%   - récupérer la liste des valeurs
%%   - récupéré la valeur associée à une clé.
%% @end
%%--------------------------------------------------------------------
handle_call(get_keys, _From, Etat) ->
    {reply, maps:keys(Etat), Etat};
handle_call(get_values, _From, Etat) ->
    {reply, maps:values(Etat), Etat};
handle_call({get, Key}, _From, Etat) ->
    {reply, maps:get(Key, Etat, undefined), Etat}.

%%--------------------------------------------------------------------
%% @doc add/3 est une interface permettant de rajouter une clé
%% associée à une valeur.
%% @end
%%--------------------------------------------------------------------
add(Pid, Key, Value) ->
    gen_server:cast(Pid, {add, Key, Value}).

%%--------------------------------------------------------------------
%% @doc delete/2 est une interface permettant de supprimer une clé
%% avec sa valeur associée.
%% @end
%%--------------------------------------------------------------------
delete(Pid, Key) ->
    gen_server:cast(Pid, {delete, Key}).

%%--------------------------------------------------------------------
%% @doc get_keys/1 permet de récupérer la liste des clés.
%% @end
%%--------------------------------------------------------------------
get_keys(Pid) ->
    gen_server:call(Pid, get_keys).

%%--------------------------------------------------------------------
%% @doc get_values/1 permet de récupérer la liste des valeurs.
%% @end
%%--------------------------------------------------------------------
get_values(Pid) ->
    gen_server:call(Pid, get_values).

%%--------------------------------------------------------------------
%% @doc get/2 permet de récupéré une valeur associée à un clé.
%% @end
%%--------------------------------------------------------------------
get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).
