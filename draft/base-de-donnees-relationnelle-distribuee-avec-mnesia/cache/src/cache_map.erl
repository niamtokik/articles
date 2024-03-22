%%%-------------------------------------------------------------------
%%% @doc le module cache permet de créer un processus permettant de
%%% stockée des clés associés à des valeurs.
%%% @end
%%%-------------------------------------------------------------------
-module(cache_map).
-behaviour(gen_server).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3]).

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
