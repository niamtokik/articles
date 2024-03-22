-module(cache_tls_acceptor).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).
-behavior(gen_server).
-record(state, { listener_sock = undefined
               , accept_sock = undefined
               , tls_sock = undefined
               }).
-define(TIMEOUT, 10000).

init(ListenerSock) ->
    {ok, #state{ listener_sock = ListenerSock }, {continue, wait}}.

terminate(_, #state{ tls_sock = TlsSock }) ->
    ssl:close(TlsSock).

handle_call(Message, _From, State) -> {stop, Message, State}.
handle_cast(Message, State) -> {stop, Message, State}.

handle_info({ssl, TlsSock, Message}, #state{ tls_sock = TlsSock } = State) ->
    {noreply, State, {continue, {command, Message}}};
handle_info({ssl_closed, TlsSock} = Message, #state{ tls_sock = TlsSock } = State) ->
    {noreply, State, {continue, closed}};
handle_info(Message, #state{ tls_sock = TlsSock } = State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% wait for tcp connection
%%--------------------------------------------------------------------
handle_continue(wait, #state{ listener_sock = ListenerSock } = State) ->
    case ssl:transport_accept(ListenerSock) of
        {ok, AcceptSock} ->
            {noreply, State#state{ accept_sock = AcceptSock }, {continue, accepted}};
        Otherwise -> 
            {stop, {wait, Otherwise}, State}
    end;
%%--------------------------------------------------------------------
%% wait for tls handshake
%%--------------------------------------------------------------------
handle_continue(accepted, #state{ accept_sock = AcceptSock } = State) ->
    case ssl:handshake(AcceptSock, ?TIMEOUT) of
        {ok, TlsSock} ->
            {noreply, State#state{ tls_sock = TlsSock }};
        Otherwise -> 
            {stop, {accepted, Otherwise}, State}
    end;
%%--------------------------------------------------------------------
%% parse the command
%%--------------------------------------------------------------------
handle_continue({command, Message}, #state{ tls_sock = TlsSock} = State) ->
    io:format("~p~n", [{?MODULE, self(), info, Message}]),
    case cache_lib:parse(Message) of
        {error, Raison} -> 
            {noreply, State, {continue, {error, Raison}}};
        {Module, Command, Args} = Call -> 
            {noreply, State, {continue, {sync, Call}}};
        Otherwise -> 
            {stop, Otherwise, State}
    end;

%%--------------------------------------------------------------------
%% error message
%%--------------------------------------------------------------------
handle_continue({error, Reason}, #state{ tls_sock = TlsSock} = State) ->
    ssl:send(TlsSock, <<"! ", Reason/bitstring, "\n">>),    
    {noreply, State, {continue, closed}};
%%--------------------------------------------------------------------
%% async command return (ok)
%%--------------------------------------------------------------------
handle_continue(async, #state{ tls_sock = TlsSock } = State) ->
    ssl:send(TlsSock, <<"ok\n">>),
    {noreply, State, {continue, closed}};
%%--------------------------------------------------------------------
%% sync command return (response)
%%--------------------------------------------------------------------
handle_continue({sync, {M,F,A} = Call}, #state{ tls_sock = TlsSock} = State) ->
    case erlang:apply(M, F, A) of
        ok -> 
            {noreply, State, {continue, async}};
        Reponse when is_bitstring(Reponse) -> 
            ssl:send(TlsSock, <<Reponse/bitstring, "\n">>),
            {noreply, State, {continue, closed}};
        Reponse when is_list(Reponse) ->
            [ ssl:send(TlsSock, <<X/bitstring,"\n">>) || X <- Reponse ],
            {noreply, State, {continue, closed}};
        Otherwise -> 
            {stop, Otherwise, State}
    end;
%%--------------------------------------------------------------------
%% close tls connection
%%--------------------------------------------------------------------
handle_continue(closed, #state{ accept_sock = AcceptSock, tls_sock = TlsSock} = State) ->
    ssl:close(TlsSock),
    NewState = State#state{ accept_sock = undefined, tls_sock = undefined},
    {noreply, NewState, {continue, wait}}.
