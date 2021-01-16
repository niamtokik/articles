# Création d'une application réseau avec Erlang

Cet article ce découpe en deux parties, la première, majoritairement
théorique, permet d'introduire les deux modules utilisés pour gérer
les connexions réseaux, `gen_tcp` ainsi que `gen_udp`.

La seconde partie est orienté projet, et permet de continuer sur
l'implémentation de l'application de cache en y intégrant une partie
réseau utilisant les exemples présent dans la première partie tout en
y rajoutant une interface en ligne de commande.

## Exemples Théoriques (première partie)

Le code lié à `gen_udp`:

 * `udp_example.erl`: présentation d'un client UDP simple
 * `udp_server.erl`: création d'un serveur UDP utilisant `gen_server`
 * `udp_client.erl`: création d'un client UDP utilisant `gen_server`
 
Le code lié à `gen_tcp`:
 
 * `tcp_example.erl`: présentation d'un client tcp simple
 * `tcp_server.erl`: création d'un serveur TCP utilisant `gen_server`
 * `tcp_client.erl`: création d'un client TCP utilisant `gen_server`

## Projet (seconde partie)

Le projet `cache` se trouve dans le répertoire `cache` et est maintenu
avec `rebar3`.

```sh
cd cache
```

Exécute les tests unitaires et d'intégration avec `eunit` et `ct`:

```sh
rebar3 eunit
rebar3 ct
```

Crée un `escript`:

```sh
rebar3 escriptize
```
