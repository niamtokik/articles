-module(cache_ssh_client).
-behavior(ssh_client_channel).
-export([connect/3, add/4, delete/3, get/3, get_keys/2, get_values/2]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2, handle_msg/2, handle_ssh_msg/2]).

connect(Host, Port, Options) ->
    {ok, Connection} = ssh:connect(Host, Port, Options),
    {ok, Channel} = ssh_connection:session_channel(Connection, 10000),
    Args = #{ connection => Connection, channel => Channel },
    ssh_client_channel:start(Connection, Channel, ?MODULE, Args).

init(#{ connection := Connection, channel := Channel } = State) -> 
    success = ssh_connection:subsystem(Connection, Channel, "cache", 10000),
    {ok, State}.

handle_call({command, Command} = _Message, From, State) ->
    send_command(Command, State),
    {noreply, State#{ from => From }}.

handle_cast({command, Command} = _Message, State) ->
    send_command(Command, State),
    {noreply, State}.

handle_info(_Message, State) -> {noreply, State}.

handle_msg({ssh_channel_up, _Channel, _Connection} = _Message, State) -> {ok, State};
handle_msg('EXIT' = _Message, State) -> {ok, State};
handle_msg(_Message, State) -> {ok, State}.

handle_ssh_msg({ssh_cm,_Connection,{data,_,_Channel,Data}} = _Message
              ,#{ from := _From, reply := Reply } = State) -> 
    {ok, State#{ reply => <<Reply/binary, Data/binary>>}};
handle_ssh_msg({ssh_cm,_Connection,{data,_,_Channel,Data}} = _Message
              ,#{ from := _From } = State) -> 
    {ok, State#{ reply => Data}};
handle_ssh_msg({ssh_cm,_Connection,{eof,_Channel}}, #{ channel := Channel } = State) ->
    {stop, Channel, State};
handle_ssh_msg(_Message, State) -> {ok, State}.

terminate(_Reason, #{ from := From, reply := Reply } = _State) ->
    DecodedReply = string:split(Reply, <<"\n">>, all),
    ssh_client_channel:reply(From, DecodedReply);
terminate(_Reason, _State) -> ok.

send_command(Command, #{ channel := Channel, connection := Connection } = _State) ->
    Data = cache_lib:join(Command),
    ssh_connection:send(Connection, Channel, <<Data/binary, "\n">>).

add(Host, Port, Key, Value) ->
    {ok, Connection} = connect(Host, Port, []),
    ssh_client_channel:cast(Connection, {command, [<<"add">>, Key, Value]}).

delete(Host, Port, Key) -> 
    {ok, Connection} = connect(Host, Port, []),
    ssh_client_channel:cast(Connection, {command, [<<"delete">>, Key]}).

get(Host, Port, Key) -> 
    {ok, Connection} = connect(Host, Port, []),
    ssh_client_channel:call(Connection, {command, [<<"get">>, Key]}).

get_keys(Host, Port) -> 
    {ok, Connection} = connect(Host, Port, []),
    ssh_client_channel:call(Connection, {command, [<<"get_keys">>]}).

get_values(Host, Port) -> 
    {ok, Connection} = connect(Host, Port, []),
    ssh_client_channel:call(Connection, {command, [<<"get_values">>]}).

