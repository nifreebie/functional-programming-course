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
    send_queue = queue:new()
}).

start_link(Sock, NodePid) ->
    io:format("~p ~n ~p",[Sock,NodePid]),
    gen_server:start_link(
        ?MODULE,
        {Sock, NodePid},
        []
    ).

init({Sock, NodePid}) ->
    {Pub, Priv} = p2p_crypto:generate_keypair(),

    Peer =
        case inet:peername(Sock) of
            {ok, P} -> P;
            _ -> unknown
        end,

    {ok, #state{
        sock       = Sock,
        peer_addr = Peer,
        our_pub   = Pub,
        our_priv  = Priv,
        node_pid  = NodePid,
        recv_buf  = <<>>,
        send_queue = queue:new()
    }}.

send_framed(Sock, Bin) when is_binary(Bin) ->
    Pack = <<(byte_size(Bin)):32/big, Bin/binary>>,
    case gen_tcp:send(Sock, Pack) of
        ok ->
            io:format("[p2p_conn] send_framed: sent ~p bytes~n", [byte_size(Pack)]),
            ok;
        {error, Reason} ->
            io:format("[p2p_conn] send_framed: gen_tcp:send error: ~p~n", [Reason]),
            {error, Reason}
    end.


-spec take_frame(binary()) -> {none, binary()} | {frame, binary(), binary()}.
take_frame(Bin) when byte_size(Bin) < 4 ->
    {none, Bin};
take_frame(<<Len:32/big, Rest/binary>>) when byte_size(Rest) < Len ->
    {none, <<Len:32/big, Rest/binary>>};
take_frame(<<Len:32/big, Rest/binary>>) ->
    <<Frame:Len/binary, Tail/binary>> = Rest,
    {frame, Frame, Tail}.

handle_info({tcp, Socket, Data}, State = #state{sock = Socket}) ->
    case byte_size(Data) =< 200 of
        false -> ok;
        true -> nott
    end,
    NewBuf = <<(State#state.recv_buf)/binary, Data/binary>>,
    process_frames(Socket, NewBuf, State#state{recv_buf = <<>>});

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

process_frames(Socket, Buf, State) ->
    case take_frame(Buf) of
        {none, Rem} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{recv_buf = Rem}};
        {frame, FrameBin, Tail} ->
            case safe_binary_to_term(FrameBin) of

                {ok, {handshake, TheirPubBin}} when is_binary(TheirPubBin) ->

                    Key =
                        p2p_crypto:compute_shared_key(
                            TheirPubBin,
                            State#state.our_priv
                        ),

                    %% save key, process tail and open socket
                    inet:setopts(Socket, [{active, once}]),
                    flush_send_queue(
                        State#state{
                            shared_key = Key,
                            recv_buf   = Tail
                        }
                    );

                {ok, {encrypted, Payload}} when is_binary(Payload) ->
                    case State#state.shared_key of
                        undefined ->
                            process_frames(Socket, Tail, State);

                        Key ->
                            case p2p_crypto:decrypt(Key, Payload) of
                                {ok, Plain} ->
                                    io:format("~p",[State]),
                                    State#state.node_pid !
                                        {incoming_msg, {State#state.peer_addr,State#state.node_pid}, Plain},

                                    process_frames(Socket, Tail, State);

                                {error, _Reason} ->
                                    process_frames(Socket, Tail, State)
                            end
                    end;
                {ok, _Other} ->
                    process_frames(Socket, Tail, State);

                {error, _Reason} ->
                    process_frames(Socket, Tail, State)
            end
    end.


safe_binary_to_term(Bin) ->
    try binary_to_term(Bin) of
        Term -> {ok, Term}
    catch
        _:Reason -> {error, Reason}
    end.

flush_send_queue(State = #state{
    sock = Sock,
    send_queue = _,
    shared_key = undefined
}) ->
    inet:setopts(Sock, [{active, once}]),
    {noreply, State};

flush_send_queue(State = #state{
    sock = Sock,
    send_queue = Q,
    shared_key = Key
}) ->
    case queue:out(Q) of
        {empty, _Q1} ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, State};

        {{value, Plain}, Q2} ->
            Enc = p2p_crypto:encrypt(Key, Plain),
            ok = send_framed(Sock, term_to_binary({encrypted, Enc})),
            flush_send_queue(State#state{send_queue = Q2})
    end.


handle_call({send_plain, TextBin}, _From, State = #state{shared_key = undefined, send_queue = Q}) ->
    NewQ = queue:in(TextBin, Q),
    {reply, {queued, queue:len(NewQ)}, State#state{send_queue = NewQ}};

handle_call({send_plain, TextBin}, _From, State = #state{shared_key = Key, sock = Sock}) ->
    Enc = p2p_crypto:encrypt(Key, TextBin),
    ok = send_framed(Sock, term_to_binary({encrypted, Enc})),
    {reply, ok, State};

handle_call(_,_From, State) ->
    {reply, ok, State}.

handle_cast(init_socket, State = #state{sock = Sock, our_pub = OurPub, peer_addr = Peer}) ->
    ok = inet:setopts(Sock, [{active, once}]),
    io:format("[p2p_conn] sending handshake to ~p~n", [Peer]),
    HsBin = term_to_binary({handshake, p2p_crypto:pubkey_to_bin(OurPub)}),
    case send_framed(Sock, HsBin) of
        ok ->
            io:format("[p2p_conn] handshake send ok to ~p~n", [Peer]);
        {error, R} ->
            io:format("[p2p_conn] handshake send ERROR ~p to ~p~n", [R, Peer])
    end,
    {noreply, State}.


terminate(_R, _State) -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.
