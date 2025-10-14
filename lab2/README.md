# Лабораторная работа 2

  * Студент: `Исупов Никита Александрович`
  * Группа: `P3331`
  * ИСУ: `408708`
  * Функциональный язык: `Erlang`

## Требования

Интерфейс — `Set`, структура данных — `OpenAdress Hashmap`.

1. Функции:
    * [x] добавление и удаление элементов;
    * [x] фильтрация;
    * [x] отображение (`map`);
    * [x] свертки (левая и правая);
    * [x] структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/%D0%9C%D0%BE%D0%BD%D0%BE%D0%B8%D0%B4).
2. Структуры данных должны быть **неизменяемыми**.
3. Библиотека должна быть протестирована в рамках **unit testing**.
4. Библиотека должна быть протестирована в рамках **property-based** тестирования (*как минимум 3 свойства*, включая свойства моноида).
5. Структура должна быть **полиморфной**.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка — необходимо реализовать их вручную и по возможности — обеспечить совместимость.


## Ключевые элементы реализации
Структура:
```erlang
-opaque t() :: {set, pos_integer(), non_neg_integer(), tuple()}.
```

Вспомогательные функции:
```erlang
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
            NewT =
                case DeletedIdx of
                    undefined -> Idx;
                    T -> T
                end,
            insert_probe_i(Table, Size, Index0, I + 1, Key, NewT);
        EntryKey when EntryKey =:= Key ->
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
```

Добавление и удаление элементов:

```erlang
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
```

Фильтрация
```erlang
-spec filter(fun((term()) -> boolean()), t()) -> t().
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
```
Отображение
```erlang
-spec map(fun((term()) -> term()), t()) -> t().
map(Fun, Set) when is_function(Fun, 1) ->
    foldl(fun(E, Acc) -> add(Fun(E), Acc) end, new(), Set).
```
Свертки
```erlang
-spec foldl(fun((term(), term()) -> term()), term(), t()) -> term().
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

-spec foldr(fun((term(), term()) -> term()), term(), t()) -> term().
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
```

Свойства моноида 
```erlang 
-spec union(t(), t()) -> t().
union(A, B) ->
    foldl(fun(E, Acc) -> add(E, Acc) end, A, B).

-spec empty() -> t().
empty() -> new().

-spec new() -> t().
new() -> new(?DEFAULT_SIZE).

-spec new(pos_integer()) -> t().
new(Size) when is_integer(Size), Size > 0 ->
    Table = make_table(Size),
    {set, Size, 0, Table}.
```

## Тесты

### Unit-тесты
* **Идемпотентность вставки**, корректный `size`.
* **filter/map/fold**: корректное содержимое, сохранение инварианта.
* корректность **equals**.

```erlang
add_remove_test() ->
    S = oaset:new(),
    S1 = oaset:add(1, S),
    ?assert(oaset:contains(S1, 1)),
    ?assert(oaset:size(S1) =:= 1),
    S2 = oaset:add(1, S1),
    ?assert(oaset:size(S2) =:= 1),
    S3 = oaset:remove(1, S1),
    ?assert(not oaset:contains(S3, 1)),
    ?assert(oaset:size(S3) =:= 0).

map_filter_fold_test() ->
    S = oaset:from_list([1, 2, 3, 4, 5]),
    S2 = oaset:map(fun(X) -> X * 2 end, S),
    ?assert(oaset:contains(S2, 2)),
    ?assert(oaset:contains(S2, 10)),
    S3 = oaset:filter(fun(X) -> X rem 2 =:= 0 end, S),
    L = oaset:to_list(S3),
    lists:foreach(fun(E) -> ?assert(E rem 2 =:= 0) end, L),
    Sum = oaset:foldl(fun(X, Acc) -> X + Acc end, 0, S),
    ?assert(Sum =:= 15).

equals_test() ->
    A = oaset:from_list([1, 2, 3]),
    B = oaset:from_list([3, 2, 1]),
    ?assert(oaset:equals(A, B)).
```

## Property-based

* Ассоциативность `union`.
* Нейтральный элемент (коммутативаность).
* `Map` + эквивалентость.

```erlang 
prop_union_associative() ->
    ?FORALL(
        {A, B, C},
        {list(integer()), list(integer()), list(integer())},
        begin
            Aset = oaset:from_list(A),
            Bset = oaset:from_list(B),
            Cset = oaset:from_list(C),
            S1 = oaset:union(Aset, oaset:union(Bset, Cset)),
            S2 = oaset:union(oaset:union(Aset, Bset), Cset),
            oaset:equals(S1, S2)
        end
    ).

prop_union_identity() ->
    ?FORALL(
        A,
        list(integer()),
        begin
            S = oaset:from_list(A),
            oaset:equals(oaset:union(S, oaset:empty()), S) andalso
                oaset:equals(oaset:union(oaset:empty(), S), S)
        end
    ).

prop_map_identity() ->
    ?FORALL(
        A,
        list(integer()),
        begin
            S = oaset:from_list(A),
            oaset:equals(oaset:map(fun(X) -> X end, S), S)
        end
    ).
```

## Выводы 
В ходе выполнения лабораторной работы были реализованы и использованы основные приёмы функционального программирования:

* Неизменяемость данных - все операции над множеством возвращают новые структуры, не изменяя исходные.
* Рекурсия применялась вместо циклов для обхода и пробирования элементов.
* Для проверки корректности структуры применены unit-тесты и property-based тестирование, включая проверку свойств моноида.