-module(oaset_utils).
-author("nifreebie").
-compile(export_all).

-define(EMPTY, empty).
-define(DELETED, deleted).

insert_into_table(Table, Size, Key) ->
    Index0 = erlang:phash2(Key, Size),
    insert_probe(Table, Size, Index0, Key, undefined).

insert_probe(Table, Size, Index0, Key, TombstoneIdx) ->
    insert_probe_i(Table, Size, Index0, 0, Key, TombstoneIdx).

insert_probe_i(Table, Size, Index0, I, Key, TombstoneIdx) when I >= Size ->
    case TombstoneIdx of
        undefined ->
            {ok, Table};
        _ ->
            NewTable = erlang:setelement(TombstoneIdx, Table, Key),
            {ok, NewTable}
    end;
insert_probe_i(Table, Size, Index0, I, Key, TombstoneIdx) ->
    Idx0 = (Index0 + I) rem Size,
    Idx = Idx0 + 1,
    Entry = element(Idx, Table),
    case Entry of
        ?EMPTY ->
            Pos =
                case TombstoneIdx of
                    undefined -> Idx;
                    T -> T
                end,
            {ok, erlang:setelement(Pos, Table, Key)};
        ?DELETED ->
            % remember first deleted and continue
            NewT =
                case TombstoneIdx of
                    undefined -> Idx;
                    T -> T
                end,
            insert_probe_i(Table, Size, Index0, I + 1, Key, NewT);
        EntryKey when EntryKey =:= Key ->
            % already present
            {ok, Table};
        _Other ->
            insert_probe_i(Table, Size, Index0, I + 1, Key, TombstoneIdx)
    end.

find_slot(Table, Size, Key) ->
    Index0 = erlang:phash2(Key, Size),
    find_probe(Table, Size, Index0, 0, Key).

find_probe(Table, Size, Index0, I, Key) ->
    Idx0 = (Index0 + I) rem Size,
    Idx = Idx0 + 1,
    Entry = element(Idx, Table),
    case Entry of
        ?EMPTY -> not_found;
        ?DELETED -> find_probe(Table, Size, Index0, I + 1, Key);
        EntryKey when EntryKey =:= Key -> {found, Idx};
        _Other -> find_probe(Table, Size, Index0, I + 1, Key)
    end.

rehash_insert_all(Table, Size, Keys) ->
    lists:foldl(
        fun(Key, Tbl) ->
            {ok, NewTbl} = insert_into_table(Tbl, Size, Key),
            NewTbl
        end,
        Table,
        Keys
    ).
