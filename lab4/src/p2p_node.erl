-module(p2p_node).
-behaviour(gen_server).

-export([start_link/1, connect/3, send_message/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    listen_socket,
    port,
    conns = #{}
}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

connect(Host, Port, Timeout) ->
    gen_server:call(?MODULE, {connect, Host, Port, Timeout}).

send_message(Node, Host, Port, Text) ->
    gen_server:call(Node, {send, Host, Port, Text}).

init([{port, Port}]) ->
    application:ensure_all_started(crypto),
    {ok, Listen} =
        gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    Self = self(),
    spawn(fun() -> acceptor(Listen, Self) end),
    {ok, #state{listen_socket = Listen, port = Port}}.

acceptor(Listen, Node) ->
    case gen_tcp:accept(Listen) of
        {ok, Sock} ->
            {ok, Conn} = p2p_conn:start_link(Sock, Node),
            gen_tcp:controlling_process(Sock, Conn),
            gen_server:cast(Conn, init_socket),
            Node ! {new_connection, Conn, Sock},
            acceptor(Listen, Node);
        _ ->
            timer:sleep(1000),
            acceptor(Listen, Node)
    end.

handle_call({connect, Host, Port, _}, _, State) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
        {ok, Sock} ->
            {ok, Conn} = p2p_conn:start_link(Sock, self()),
            gen_tcp:controlling_process(Sock, Conn),
            gen_server:cast(Conn, init_socket),
            erlang:monitor(process, Conn),
            {reply, ok,
             State#state{conns = maps:put({Host, Port}, Conn,
                                          State#state.conns)}};
        Error ->
            {reply, Error, State}
    end;

handle_call({send, Host, Port, Text}, _, State) ->
    Key = {Host, Port},
    case maps:get(Key, State#state.conns, undefined) of
        undefined ->
            handle_call({connect, Host, Port, 0}, self(), State),
            handle_call({send, Host, Port, Text}, self(), State);
        Conn ->
            Reply = gen_server:call(Conn, {send_plain, Text}),
            {reply, Reply, State}
    end;

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({incoming_msg, Peer, Plain}, State) ->
    Ts = calendar:system_time_to_rfc3339(
        erlang:system_time(seconds), [{unit, second}]),
    p2p_history:persist({self(), Peer, Ts, incoming, Plain}),
    io:format("<< [~s] ~p: ~s~n", [Ts, Peer, Plain]),
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    NewConns =
        maps:filter(fun(_, V) -> V =/= Pid end, State#state.conns),
    {noreply, State#state{conns = NewConns}};

handle_info({new_connection, Conn, Sock}, State) ->
    erlang:monitor(process, Conn),
    case inet:peername(Sock) of
        {ok, Peer} ->
            {noreply,
             State#state{conns =
                 maps:put(Peer, Conn, State#state.conns)}};
        _ ->
            {noreply, State}
    end;

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
