-module(p2p_conn).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    sock,
    peer_addr,
    our_pub,
    our_priv,
    shared_key = undefined,
    node_pid,
    recv_buf = <<>>,
    send_queue
}).

start_link(Sock, NodePid) ->
    gen_server:start_link(?MODULE, {Sock, NodePid}, []).

init({Sock, NodePid}) ->
    {Pub, Priv} = p2p_crypto:generate_keypair(),
    Peer =
        case inet:peername(Sock) of
            {ok, P} -> P;
            _ -> unknown
        end,
    {ok, #state{
        sock = Sock,
        peer_addr = Peer,
        our_pub = Pub,
        our_priv = Priv,
        node_pid = NodePid,
        recv_buf = <<>>,
        send_queue = queue:new()
    }}.

send_framed(Sock, Bin) when is_binary(Bin) ->
    Pack = <<(byte_size(Bin)):32/big, Bin/binary>>,
    case gen_tcp:send(Sock, Pack) of
        ok -> ok;
        {error, Reason} ->
            io:format("[p2p_conn][error] send failed: ~p~n", [Reason]),
            {error, Reason}
    end.

take_frame(Bin) when byte_size(Bin) < 4 ->
    {none, Bin};
take_frame(<<Len:32/big, Rest/binary>>) when byte_size(Rest) < Len ->
    {none, <<Len:32/big, Rest/binary>>};
take_frame(<<Len:32/big, Rest/binary>>) ->
    <<Frame:Len/binary, Tail/binary>> = Rest,
    {frame, Frame, Tail}.

handle_info({tcp, Socket, Data}, State = #state{sock = Socket}) ->
    NewBuf = <<(State#state.recv_buf)/binary, Data/binary>>,
    process_frames(Socket, NewBuf, State#state{recv_buf = <<>>});

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    {stop, Reason, State};

handle_info(_, State) ->
    {noreply, State}.

process_frames(Socket, Buf, State) ->
    case take_frame(Buf) of
        {none, Rem} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{recv_buf = Rem}};
        {frame, FrameBin, Tail} ->
            case safe_binary_to_term(FrameBin) of
                {ok, {handshake, TheirPubBin}} ->
                    Key = p2p_crypto:compute_shared_key(
                        TheirPubBin,
                        State#state.our_priv
                    ),
                    inet:setopts(Socket, [{active, once}]),
                    io:format("[p2p_conn] secure channel established with ~p~n",
                              [State#state.peer_addr]),
                    flush_send_queue(
                        State#state{
                            shared_key = Key,
                            recv_buf = Tail
                        }
                    );
                {ok, {encrypted, Payload}} ->
                    case State#state.shared_key of
                        undefined ->
                            process_frames(Socket, Tail, State);
                        Key ->
                            case p2p_crypto:decrypt(Key, Payload) of
                                {ok, Plain} ->
                                    State#state.node_pid !
                                        {incoming_msg,
                                         {State#state.peer_addr,
                                          State#state.node_pid},
                                         Plain},
                                    process_frames(Socket, Tail, State);
                                _ ->
                                    process_frames(Socket, Tail, State)
                            end
                    end;
                _ ->
                    process_frames(Socket, Tail, State)
            end
    end.

safe_binary_to_term(Bin) ->
    try binary_to_term(Bin) of
        Term -> {ok, Term}
    catch
        _:_ -> {error, invalid}
    end.

flush_send_queue(State = #state{shared_key = undefined, sock = Sock}) ->
    inet:setopts(Sock, [{active, once}]),
    {noreply, State};

flush_send_queue(State = #state{sock = Sock, send_queue = Q, shared_key = Key}) ->
    case queue:out(Q) of
        {empty, _} ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, State};
        {{value, Plain}, Q2} ->
            Enc = p2p_crypto:encrypt(Key, Plain),
            send_framed(Sock, term_to_binary({encrypted, Enc})),
            flush_send_queue(State#state{send_queue = Q2})
    end.

handle_call({send_plain, Text}, _From,
            State = #state{shared_key = undefined, send_queue = Q}) ->
    NewQ = queue:in(Text, Q),
    {reply, {queued, queue:len(NewQ)}, State#state{send_queue = NewQ}};

handle_call({send_plain, Text}, _From,
            State = #state{shared_key = Key, sock = Sock}) ->
    Enc = p2p_crypto:encrypt(Key, Text),
    Reply = send_framed(Sock, term_to_binary({encrypted, Enc})),
    {reply, Reply, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(init_socket, State = #state{sock = Sock, our_pub = Pub}) ->
    inet:setopts(Sock, [{active, once}]),
    send_framed(Sock,
        term_to_binary({handshake, p2p_crypto:pubkey_to_bin(Pub)})),
    {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
