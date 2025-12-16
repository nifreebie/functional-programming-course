-module(p2p_cli).
-export([main/1, start/1]).

main(Args) -> start(Args).

start(Args) ->
    case Args of
        [PortStr] ->
            case string:to_integer(PortStr) of
                {Port, []} when is_integer(Port), Port > 0 ->
                    start_cli(Port);
                _ ->
                    io:format("Wrong port: ~s~nUsage: escript p2p_cli <port>~n", [PortStr]),
                    halt(1)
            end;
        _ ->
            io:format("Usage: escript p2p_cli <port>~n"),
            halt(1)
    end.

start_cli(Port) ->
    ok = p2p_history:start(),
    case p2p_node:start_link([{port, Port}]) of
        {ok, NodePid} ->
            io:format("P2P node started on port ~p. Type 'help' for commands.~n", [Port]),
            repl(NodePid);
        {error, Reason} ->
            io:format("Unable to start node: ~p~n", [Reason]),
            halt(1)
    end.

repl(NodePid) ->
    prompt(),
    case io:get_line("") of
        eof ->
            io:format("Bye~n"),
            ok;
        Line ->
            CmdLine = string:trim(Line),
            case parse_cmd(CmdLine) of
                {connect, Host, Port} ->
                    do_connect(Host, Port),
                    repl(NodePid);
                {send, Host, Port, Text} ->
                    do_send(NodePid, Host, Port, Text),
                    repl(NodePid);
                history ->
                    do_history(),
                    repl(NodePid);
                help ->
                    print_help(),
                    repl(NodePid);
                quit ->
                    io:format("Bye~n"),
                    ok;
                unknown ->
                    io:format("Unknown command. Type 'help' for commands.~n"),
                    repl(NodePid)
            end
    end.

prompt() -> io:format("> ").

parse_cmd(CmdLine) ->
    case string:tokens(CmdLine, " ") of
        ["connect", Host, PortStr] ->
            case string:to_integer(PortStr) of
                {Port, []} when is_integer(Port) -> {connect, Host, Port};
                _ -> unknown
            end;
        ["send", Host, PortStr | Rest] when Rest =/= [] ->
            case string:to_integer(PortStr) of
                {Port, []} when is_integer(Port) ->
                    Text = string:join(Rest, " "),
                    {send, Host, Port, Text};
                _ -> unknown
            end;
        ["history"] -> history;
        ["help"] -> help;
        ["quit"] -> quit;
        [] -> unknown;
        _ -> unknown
    end.

do_connect(Host, Port) ->
    io:format("Connecting to ~s:~p ...~n", [Host, Port]),
    case p2p_node:connect(Host, Port, 5000) of
        {ok, _ConnPid} -> io:format("Connected to ~s:~p~n", [Host, Port]);
        ok -> io:format("Connected to ~s:~p~n", [Host, Port]);
        {error, Reason} -> io:format("Connect error: ~p~n", [Reason]);
        Other -> io:format("Connect returned unexpected: ~p~n", [Other])
    end.

do_send(NodePid, Host, Port, Text) ->
    Bin = list_to_binary(Text),
    case p2p_node:send_message(NodePid, Host, Port, Bin) of
        ok ->
            p2p_history:persist({self(), {Host,Port}, human_ts(), outgoing, Bin}),
            io:format("Sent to ~s:~p~n", [Host, Port]);
        {queued, _Len} ->
            p2p_history:persist({self(), {Host,Port}, human_ts(), outgoing, Bin}),
            io:format("Queued (handshake pending) to ~s:~p~n", [Host, Port]);
        {error, Reason} ->
            io:format("Send error: ~p~n", [Reason]);
        Other ->
            p2p_history:persist({self(), {Host,Port}, human_ts(), outgoing, Bin}),
            io:format("Send returned: ~p~n", [Other])
    end.

do_history() ->
    Hist = p2p_history:load_all(),
    case Hist of
        [] -> io:format("History is empty.~n");
        _ ->
            lists:foreach(fun(M) ->
                case M of
                    {_FromPid, Peer, Ts, Direction, Bin} ->
                        DirStr = case Direction of incoming -> "<<"; outgoing -> ">>"; _ -> "??" end,
                        Text = case is_binary(Bin) of true -> binary_to_list(Bin); false -> io_lib:format("~p", [Bin]) end,
                        io:format("~s ~p : ~s~n", [DirStr, Peer, Text]);
                    _ -> io:format("~p~n", [M])
                end
            end, Hist)
    end.

print_help() ->
    io:format("Commands:\n"),
    io:format("  connect <host> <port>        - open TCP connection to peer~n"),
    io:format("  send <host> <port> <text>    - send text message~n"),
    io:format("  history                      - show local chat history~n"),
    io:format("  help                         - this help~n"),
    io:format("  quit                         - exit~n").

human_ts() ->
    Ts = erlang:system_time(seconds),
    case calendar:system_time_to_rfc3339(Ts, [{unit, second}]) of
        S when is_list(S) -> S;
        Bin when is_binary(Bin) -> binary_to_list(Bin)
    end.
