-module(p2p_history).
-export([start/0, stop/0, persist/1, load_all/0]).

-define(HISTORY_FILE, "history.dat").

start() -> ok.
stop() -> ok.

persist(Message) ->
    Bin = term_to_binary(Message),
    Len = byte_size(Bin),
    case file:open(?HISTORY_FILE, [append, {raw, true}]) of
        {ok, File} ->
            file:write(File, <<Len:32/big, Bin/binary>>),
            file:close(File),
            ok;
        Error -> Error
    end.

load_all() ->
    case file:read_file(?HISTORY_FILE) of
        {ok, Bin} -> load_from_bin(Bin, []);
        _ -> []
    end.

load_from_bin(<<>>, Acc) ->
    lists:reverse(Acc);
load_from_bin(<<Len:32/big, Rest/binary>>, Acc)
  when byte_size(Rest) >= Len ->
    <<MsgBin:Len/binary, Tail/binary>> = Rest,
    load_from_bin(Tail, [binary_to_term(MsgBin) | Acc]);
load_from_bin(_, Acc) ->
    lists:reverse(Acc).
