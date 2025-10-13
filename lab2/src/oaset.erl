-module(oaset).
-author("nifeebie").

%% API
-export([
    new/0,
    add/2,
    remove/2,
    contains/2,
    filter/2,
    map/2,
    foldl/3,
    foldr/3,
    union/2,
    empty/0,
    equals/2,
    size/1,
    to_list/1,
    from_list/1
]).

-import(oaset_utils, [
    insert_into_table/3,
    insert_probe/5,
    find_slot/3,
    rehash_insert_all/3
]).

-opaque t() :: {set, pos_integer(), non_neg_integer(), tuple()}.

-define(DEFAULT_SIZE, 8).
-define(MAX_LOAD, 0.7).
-define(DELETED, deleted).
-define(EMPTY, empty).

-spec new() -> t().
new() -> new(?DEFAULT_SIZE).

-spec new(pos_integer()) -> t().
new(Size) when is_integer(Size), Size > 0 ->
    Table = make_table(Size),
    {set, Size, 0, Table}.

make_table(Size) ->
    list_to_tuple(lists:duplicate(Size, ?EMPTY)).

-spec empty() -> t().
empty() -> new().

to_list(S) ->
    lists:reverse(foldl(fun(E, Acc) -> [E | Acc] end, [], S)).

-spec from_list([term()]) -> t().
from_list(List) when is_list(List) ->
    lists:foldl(fun add/2, new(), List).

-spec size(t()) -> non_neg_integer().
size({set, _Size, Count, _Table}) -> Count.

-spec contains(t(), term()) -> boolean().
contains(Set = {set, Size, _Count, Table}, Key) ->
    case find_slot(Table, Size, Key) of
        {found, _Idx} -> true;
        _ -> false
    end.

-spec add(term(), t()) -> t().
add(Key, Set = {set, Size, Count, Table}) ->
    case contains(Set, Key) of
        true ->
            Set;
        false ->
            NewCount = Count + 1,
            Load = NewCount / Size,
            if
                Load > ?MAX_LOAD ->
                    NewSize = Size * 2,
                    NewTable = make_table(NewSize),
                    Rehashed = rehash_insert_all(NewTable, NewSize, to_list(Set) ++ [Key]),
                    {set, NewSize, NewCount, Rehashed};
                true ->
                    {ok, Table2} = insert_into_table(Table, Size, Key),
                    {set, Size, NewCount, Table2}
            end
    end.

-spec remove(term(), t()) -> t().
remove(Key, Set = {set, Size, Count, Table}) ->
    case find_slot(Table, Size, Key) of
        {found, Idx} ->
            Table2 = erlang:setelement(Idx, Table, ?DELETED),
            {set, Size, Count - 1, Table2};
        _ ->
            Set
    end.

filter(Pred, Set) when is_function(Pred, 1) ->
    foldl(
        fun(E, Acc) ->
            case Pred(E) of
                true -> add(E, Acc);
                _ -> Acc
            end
        end,
        new(),
        Set
    ).

map(Fun, Set) when is_function(Fun, 1) ->
    foldl(fun(E, Acc) -> add(Fun(E), Acc) end, new(), Set).

foldl(F, Acc0, {set, Size, _Count, Table}) when is_function(F, 2) ->
    foldl_idx(1, Size, Table, F, Acc0).

foldl_idx(I, Size, _Table, _F, Acc) when I > Size -> Acc;
foldl_idx(I, Size, Table, F, Acc) ->
    Entry = element(I, Table),
    Acc2 =
        case Entry of
            ?EMPTY -> Acc;
            ?DELETED -> Acc;
            Key -> F(Key, Acc)
        end,
    foldl_idx(I + 1, Size, Table, F, Acc2).

foldr(F, Acc0, {set, Size, _Count, Table}) when is_function(F, 2) ->
    foldr_idx(Size, Table, F, Acc0).

foldr_idx(I, _Table, _F, Acc) when I < 1 -> Acc;
foldr_idx(I, Table, F, Acc) ->
    Entry = element(I, Table),
    Acc2 =
        case Entry of
            ?EMPTY -> Acc;
            ?DELETED -> Acc;
            Key -> F(Key, Acc)
        end,
    foldr_idx(I - 1, Table, F, Acc2).

union(A, B) ->
    foldl(fun(E, Acc) -> add(E, Acc) end, A, B).

equals(A = {set, _Sa, CountA, _Ta}, B = {set, _Sb, CountB, _Tb}) ->
    case CountA =:= CountB of
        false -> false;
        true -> lists:all(fun(E) -> contains(B, E) end, to_list(A))
    end.
