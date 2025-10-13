-module(oaset_utils).
-author("nifreebie").
-compile(export_all).

-define(EMPTY, empty).
-define(DELETED, deleted).

insert_into_table(Table, Size, Key) ->
    Index0 = erlang:phash2(Key, Size),
    insert_probe(Table, Size, Index0, Key, undefined).

insert_probe(Table, Size, Index0, Key, DeletedIdx) ->
    insert_probe_i(Table, Size, Index0, 0, Key, DeletedIdx).

insert_probe_i(Table, Size, Index0, I, Key, DeletedIdx) when I >= Size ->
    case DeletedIdx of
        undefined ->
            {ok, Table};
        _ ->
            NewTable = erlang:setelement(DeletedIdx, Table, Key),
            {ok, NewTable}
    end;
insert_probe_i(Table, Size, Index0, I, Key, DeletedIdx) ->
    Idx0 = (Index0 + I) rem Size,
    Idx = Idx0 + 1,
    Entry = element(Idx, Table),
    case Entry of
        ?EMPTY ->
            Pos =
                case DeletedIdx of
                    undefined -> Idx;
                    T -> T
                end,
            {ok, erlang:setelement(Pos, Table, Key)};
        ?DELETED ->
            % remember first deleted and continue
            NewT =
                case DeletedIdx of
                    undefined -> Idx;
                    T -> T
                end,
            insert_probe_i(Table, Size, Index0, I + 1, Key, NewT);
        EntryKey when EntryKey =:= Key ->
            % already present
            {ok, Table};
        _Other ->
            insert_probe_i(Table, Size, Index0, I + 1, Key, DeletedIdx)
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
