-module(p2p_history).
-export([start/0, stop/0, persist/1, load_all/0]).

-define(HISTORY_FILE, "history.dat").

start() ->
    ok.

stop() ->
    ok.

% 4 byte big-endian
persist(Message) ->
    Bin = term_to_binary(Message),
    Len = byte_size(Bin),
    case file:open(?HISTORY_FILE, [append, {raw, true}]) of
        {ok, File} ->
            ok = file:write(File, <<Len:32/big, Bin/binary>>),
            file:close(File),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

load_all() ->
    case file:read_file_info(?HISTORY_FILE) of
        {ok, _Info} ->
            case file:read_file(?HISTORY_FILE) of
                {ok, Bin} ->
                    load_all_from_bin(Bin, []);
                {error, _} -> []
            end;
        {error, _} -> []
    end.

load_all_from_bin(<<>>, Acc) -> lists:reverse(Acc);
load_all_from_bin(Bin, Acc) when byte_size(Bin) >= 4 ->
    <<Len:32/big, Rest/binary>> = Bin,
    case byte_size(Rest) >= Len of
        true ->
            <<RecBin:Len/binary, Tail/binary>> = Rest,
            Msg = binary_to_term(RecBin),
            load_all_from_bin(Tail, [Msg|Acc]);
        false ->
            lists:reverse(Acc)
    end;
load_all_from_bin(_, Acc) -> lists:reverse(Acc).
