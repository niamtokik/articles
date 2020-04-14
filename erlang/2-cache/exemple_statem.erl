%%%-------------------------------------------------------------------
%%% @doc exemple_statem permet de montrer l'utilisation du behaviour
%%% gen_statem en implémentant un interrupteur. Une implémentation
%%% similaire est donnée dans la documentation officielle.
%%% @end
%%%-------------------------------------------------------------------
-module(exemple_statem).
-export([init/1, terminate/3]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([start/1, start/2, start/0, start/3]).
-export([start_link/1, start_link/2, start_link/0, start_link/3]).
-export([appuyer/1, lampe/1]).

%%--------------------------------------------------------------------
%% @doc start/0 est une fonction pour simplifié le démarrage. Elle se
%% base sur start/1.
%% @end
%% --------------------------------------------------------------------
start() ->
    start([], []).

%%--------------------------------------------------------------------
%% @doc start/1 est une fonction pour simplifié le démarrage et se
%% base sur start/2 avec des options par défaut.
%% @end
%% --------------------------------------------------------------------
start(Arguments) ->
    start(Arguments, []).

%%--------------------------------------------------------------------
%% @doc start/2 est une fonction qui s'appuie sur gen_statem:start/3
%% pour démarrer le module exemple_statem.
%% @end
%% --------------------------------------------------------------------
start(Arguments, Options) ->
    gen_statem:start(?MODULE, Arguments, Options).

%%--------------------------------------------------------------------
%% @doc start/3 est une fonction qui s'appuie sur gen_statem:start/4,
%% permettant de démarrer un processus enregistré localement. C'est à
%% dire qu'un autre processus avec le même nom peut-être lancé sur un
%% autre noeud du cluster.
%% @end
%%--------------------------------------------------------------------
start(Nom, Arguments, Options) ->
    gen_statem:start({local, Nom}, Arguments, Options).

%%--------------------------------------------------------------------
%% @doc start_link/0 est une fonction pour simplifier le démarrer de
%% exemple_statem en créant un lien. Cette fonction s'appuie sur
%% start_link/1.
%% @end
%% --------------------------------------------------------------------
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc start_link/1 est une fonction pour simplifier le démarrer de
%% exemple_statem en créant un lien. Cette fonction s'appuie sur
%% start_link/2.
%% @end
%%--------------------------------------------------------------------
start_link(Arguments) ->
    start_link(Arguments, []).

%%--------------------------------------------------------------------
%% @doc start_link/2 s'appuie sur la fonctio gen_statem:start_link/3
%% pour démarrer le module exemple_statem pour créer un lien entre le
%% processus qui le démarre et celui créé.
%% @end
%%--------------------------------------------------------------------
start_link(Arguments, Options) ->
    gen_statem:start_link(?MODULE, Arguments, Options).

%%--------------------------------------------------------------------
%% @doc start_link/3 s'appuie sur la fonction gen_statem:start_link/4
%% et permet de démarrer un processus nommé au sein d'un cluster de
%% noeud. 
%% @end
%% --------------------------------------------------------------------
start_link(Nom, Arguments, Options) ->
    gen_statem:start_link({local, Nom}, Arguments, Options).

%%--------------------------------------------------------------------
%% @doc init/1 permet d'initialiser l'état du processus ainsi que les
%% données associées. Cette fonction est équivalent à un constructeur
%% en orienté objet.
%% @end
%%--------------------------------------------------------------------
init(_Arguments) ->
    io:format("interrupteur ouvert~n"),
    {ok, ouvert, eteint}.

%%--------------------------------------------------------------------
%% @doc terminate/3 permet de "détruire" le processus. Équivalent à un
%% destructeur en orienté objet.  
%% @end
%%--------------------------------------------------------------------
terminate(Raison, Etat, Donnee) ->
    io:format("interrupteur détruit.~n"),
    io:format("raison: ~p~n", [Raison]),
    io:format("état: ~p~n", [Etat]),
    io:format("lampe: ~p~n", [Donnee]),
    ok.

%%--------------------------------------------------------------------
%% @doc callback_mode/0 est un callback obligatoire permettant de
%% définir le fonction du module basé sur le behaviour gen_statem.
%% @end
%%--------------------------------------------------------------------
callback_mode() -> [handle_event_function].

%%--------------------------------------------------------------------
%% @doc handle_event/4 est une fonction utilisé pour récupéré les
%% différents évènements envoyé sur le processus. Ce callback est
%% défini ici due au mode du callback définis dans callback_mode/0.
%% @end
%%--------------------------------------------------------------------
handle_event(cast, appuyer, ferme, allume) ->
    io:format("lampe éteinte.~n"),
    {next_state, ouvert, eteint};
handle_event(cast, appuyer, ouvert, eteint) ->
    io:format("lampe allumée.~n"),
    {next_state, ferme, allume};
handle_event({call, From}, lampe, _Etat, Donnee) ->
    {keep_state, Donnee, [{reply, From, Donnee}]};
handle_event(TypeEvenement, Evenement, Etat, Donnee) ->
    io:format("Type évènement: ~p~n", [TypeEvenement]),
    io:format("Contenu évènement: ~p~n", [Evenement]),
    io:format("État: ~p~n", [Etat]),
    io:format("État (données) processus: ~p~n", [Donnee]),
    {keep_state, Donnee}.

%%--------------------------------------------------------------------
%% @doc appuyer/1 est une fonction exporté servant d'API. Elle se base
%% sur la fonction gen_statem:cast/2 pour envoyer un message au
%% processus.
%% @end
%% --------------------------------------------------------------------
appuyer(Pid) ->
    gen_statem:cast(Pid, appuyer).

%%--------------------------------------------------------------------
%% @doc tout comme appuyer/1, cette fonction est exporté et sert d'API
%% pour envoyer un message basé sur gen_statem:call/3.
%% @end
%% --------------------------------------------------------------------
lampe(Pid) ->
    gen_statem:call(Pid, lampe, 1000).
