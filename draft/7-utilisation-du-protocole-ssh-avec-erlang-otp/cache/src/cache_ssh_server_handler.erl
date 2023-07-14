-module(cache_ssh_server_handler).
-behaviour(ssh_server_channel).
-export([init/1, handle_msg/2, handle_ssh_msg/2]).
-compile(export_all).

init(Args) -> {ok, []}.

handle_msg(Msg, State) -> {ok, State}.

handle_ssh_msg({ssh_cm, ConnectionRef,{data, _, ChannelId, Command}}, State) ->
    parse(ConnectionRef, ChannelId, Command, State);
handle_ssh_msg(Msg, State) ->
    {ok, State}.

parse(ConnectionRef, ChannelId, Command, State) ->
    case cache_lib:parse(Command) of
        {M, F, A} = Call -> 
            execute(ConnectionRef, ChannelId, Call, State);
        {error, Raison} -> 
            execution_end(ConnectionRef, ChannelId, State);
        Otherwise -> 
            execution_end(ConnectionRef, ChannelId, State)
    end.

execute(ConnectionRef, ChannelId, {M, F, A} = Call, State) ->
    case erlang:apply(M, F, A) of
        ok -> 
            ssh_connection:send(ConnectionRef, ChannelId, <<"ok\n">>);
        Reponse when is_bitstring(Reponse) -> 
            ssh_connection:send(ConnectionRef, ChannelId, <<Reponse/bitstring, "\n">>);
        Reponse when is_list(Reponse) ->
            Fun = fun(X) -> 
              ssh_connection:send(ConnectionRef, ChannelId, <<X/bitstring, "\n">>) 
            end,
            lists:map(Fun, Reponse)
    end,
    execution_end(ConnectionRef, ChannelId, State).

execution_end(ConnectionRef, ChannelId, State) ->
    ssh_connection:close(ConnectionRef, ChannelId),
    {ok, State}.
