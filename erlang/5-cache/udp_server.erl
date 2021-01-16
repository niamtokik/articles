%%%
%%% jot 100 1 | xargs -I%i -P100 sh -c "printf -- 'add 1 2' | nc -w 1 -u 127.0.0.1 8888"
%%%
-module(udp_server).
-export([start/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").

start(Args) ->
    gen_server:start(?MODULE, Args, [debug]).

init(Port) 
  when is_integer(Port)->
    Opts = [{mode, binary}],
    {ok, Interface} = gen_udp:open(Port, Opts),
    ?LOG_DEBUG("Listen on ~p with interface ~p", [Port, Interface]),
    {ok, Interface}.

terminate(_Reason, Interface) ->
    gen_udp:close(Interface).

handle_cast(Message, Process) ->
    ?LOG_DEBUG("received cast: ~p", [Message]),
    {noreply, Process}.

handle_call(Message, From, Process) ->
    ?LOG_DEBUG("received call from ~p: ~p", [From, Message]),
    {reply, ok, Process}.

handle_info({udp, Process, Source, Port, Message} = Data, Process) ->
    ?LOG_DEBUG("received ~p from ~p:~p", [Message, Source, Port]),
    gen_udp:send(Process, Source, Port, <<"echo: ", Message/bitstring>>),
    % cache_lib:command(Data),
    {noreply, Process};
handle_info(Message, Process) ->
    ?LOG_DEBUG("received info: ~p", [Message]),
    {noreply, Process}.
    


