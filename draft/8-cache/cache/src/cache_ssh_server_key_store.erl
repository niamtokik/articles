-module(cache_ssh_server_key_store).
-export([start_link/0, get/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-behavior(gen_server). % 🅑

start_link() -> gen_server:start_link({local,?MODULE},?MODULE, [], []).

init(_) -> {ok, #{}}.

handle_call({get, 'rsa-sha2-256'}, _, Store) -> update_or_fetch('ssh-rsa', Store);
handle_call({get, 'rsa-sha2-512'}, _, Store) -> update_or_fetch('ssh-rsa', Store);
handle_call({get, Type}, _, Store) -> update_or_fetch(Type, Store);
handle_call(_, _, Store) -> {noreply, Store}.

handle_cast(_, Store) -> {noreply, Store}.
handle_info(_, Store) -> {noreply, Store}.

-define(UPDATE_OR_FETCH(TYPE, GENERATE), 
        update_or_fetch(TYPE, #{ TYPE := Key} = Store) -> % 🅒
               {reply, {ok, Key}, Store};
        update_or_fetch(TYPE, Store) -> % 🅓
               Key = public_key:generate_key(GENERATE),
               {reply, {ok, Key}, Store#{ TYPE => Key }}).

% update_or_fetch('ssh-rsa', #{ 'ssh-rsa' := Key} = Store) ->
%   {reply, {ok, Key}, Store};
% update_or_fetch('ssh-rsa', Store) ->
%   Key = public_key:generate_key({rsa, 4096, 65537}),
%  {reply, {ok, Key}, Store#{ TYPE => Key }}

?UPDATE_OR_FETCH('ssh-rsa', {rsa, 4096, 65537});
?UPDATE_OR_FETCH('ecdsa-sha2-nistp256', {namedCurve, 'secp256r1'});
?UPDATE_OR_FETCH('ecdsa-sha2-nistp384', {namedCurve, 'secp384r1'});
?UPDATE_OR_FETCH('ecdsa-sha2-nistp521', {namedCurve, 'secp521r1'});
?UPDATE_OR_FETCH('ssh-ed25519', {namedCurve, ed25519});
update_or_fetch(_,Store) -> {reply, {error, notfound}, Store}. % 🅔

get(Type) -> gen_server:call(?MODULE, {get, Type}, 10000).
