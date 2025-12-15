-module(p2p_node).
-behaviour(gen_server).
-export([start_link/1, connect/3, send_message/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    listen_socket,
    port,
    our_pub,
    our_priv,
    conns = #{}  %%  Key -> ConnPid
}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

connect(Host, Port, TimeoutMs) ->
    gen_server:call(?MODULE, {connect, Host, Port, TimeoutMs}).

send_message(Node, Host, Port, TextBin) ->
    gen_server:call(Node, {send, Host, Port, TextBin}).

init([{port, Port}]) ->
    application:ensure_all_started(crypto),
    {Pub, Priv} = p2p_crypto:generate_keypair(),

    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),

    Parent = self(),
    spawn(fun() -> acceptor(Listen, Parent) end),

    {ok, #state{listen_socket = Listen, port = Port, our_pub = Pub, our_priv = Priv}}.

acceptor(Listen, NodePid) ->
    case gen_tcp:accept(Listen) of
        {ok, Sock} ->
            {ok, ConnPid} = p2p_conn:start_link(Sock, NodePid),
            ok = gen_tcp:controlling_process(Sock, ConnPid),
            gen_server:cast(ConnPid, init_socket),

            NodePid ! {new_connection, ConnPid, Sock},
            
            acceptor(Listen, NodePid);
        {error, Reason} ->
            io:format("acceptor error: ~p~n", [Reason]),
            timer:sleep(1000),
            acceptor(Listen, NodePid)
    end.


handle_call({connect, Host, Port, _Timeout}, _From, State) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
        {ok, Sock} ->
            {ok, ConnPid} = p2p_conn:start_link(Sock, self()),
            ok = gen_tcp:controlling_process(Sock, ConnPid),
            gen_server:cast(ConnPid, init_socket),

            erlang:monitor(process, ConnPid),
            
            Key = {Host, Port},
            NewConns = maps:put(Key, ConnPid, State#state.conns),
            {reply, {ok, ConnPid}, State#state{conns = NewConns}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({send, Host, Port, TextBin}, _From, State) ->
    Key = {Host, Port},
    case maps:get(Key, State#state.conns, undefined) of
        undefined ->
            case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
                {ok, Sock} ->
                    {ok, ConnPid} = p2p_conn:start_link(Sock, self()),
                    ok = gen_tcp:controlling_process(Sock, ConnPid),
                    gen_server:cast(ConnPid, init_socket),

                    erlang:monitor(process, ConnPid),
                    
                    NewConns = maps:put(Key, ConnPid, State#state.conns),
                    Reply = gen_server:call(ConnPid, {send_plain, TextBin}),
                    {reply, Reply, State#state{conns = NewConns}};
                {error, R} ->
                    {reply, {error, R}, State}
            end;
        ConnPid ->
            case is_process_alive(ConnPid) of
                true ->
                    Reply = gen_server:call(ConnPid, {send_plain, TextBin}),
                    {reply, Reply, State};
                false ->
                    io:format("[p2p_node] Process ~p is dead, reconnecting...~n", [ConnPid]),
                    NewState = State#state{conns = maps:remove(Key, State#state.conns)},
                    handle_call({send, Host, Port, TextBin}, _From, NewState)
            end
    end;


handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    io:format("[p2p_node] Connection process ~p died: ~p~n", [Pid, Reason]),
    NewConns = maps:filter(fun(_Key, ConnPid) -> ConnPid =/= Pid end, State#state.conns),
    {noreply, State#state{conns = NewConns}};

handle_info({new_connection, ConnPid, Sock}, State) ->
    erlang:monitor(process, ConnPid),
    PeerKey = case inet:peername(Sock) of
        {ok, Peer} -> Peer;
        _ -> unknown
    end,
    NewConns = case PeerKey of
        unknown -> State#state.conns;
        _ -> maps:put(PeerKey, ConnPid, State#state.conns)
    end,
    {noreply, State#state{conns = NewConns}};


handle_info({incoming_msg, Peer, Plain}, State) ->
    Ts = erlang:system_time(seconds),
    DateStr = calendar:system_time_to_rfc3339(Ts, [{unit, second}]),
    p2p_history:persist({self(), Peer, DateStr, incoming, Plain}),
    io:format("<< From ~p @ at ~s message: ~s~n", [Peer, DateStr, Plain]),
    {noreply, State};

handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
