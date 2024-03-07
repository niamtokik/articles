-module(cache_ssh_server_key_handler).
-export([host_key/2, is_auth_key/3]).
-behaviour(ssh_server_key_api).

host_key(Type, DaemonOptions) -> cache_ssh_server_key_store:get(Type).

is_auth_key(PublicUserKey, User, DaemonOptions) -> true.
