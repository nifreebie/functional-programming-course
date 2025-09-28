-module(task1).
-author("nifreebie").

-export([
    sum_multiples_rec/1,
    sum_multiples_tail/1,
    sum_multiples_module/1,
    sum_multiples_map/1,
    sum_multiples_comp/1
]).

%% НЕ хвостовая рекурсия
sum_multiples_rec(0) ->
    0;
sum_multiples_rec(N) when (N rem 3 =:= 0 orelse N rem 5 =:= 0), N < 1000 ->
    N + sum_multiples_rec(N - 1);
sum_multiples_rec(N) ->
    sum_multiples_rec(N - 1).

%% Хвостовая рекурсия
sum_multiples_tail(N) ->
    loop(N - 1, 0).

loop(0, Acc) ->
    Acc;
loop(N, Acc) when N rem 3 =:= 0; N rem 5 =:= 0 ->
    loop(N - 1, Acc + N);
loop(N, Acc) ->
    loop(N - 1, Acc).

% Модульная реализация
sum_multiples_module(N) when is_integer(N), N >= 0 ->
    Seq = lists:seq(1, N - 1),
    Multiples = lists:filter(
        fun(X) -> (X rem 3) =:= 0 orelse (X rem 5) =:= 0 end,
        Seq
    ),
    lists:foldl(
        fun(X, Acc) -> Acc + X end,
        0,
        Multiples
    ).

% Использование map
sum_multiples_map(N) ->
    Seq = lists:seq(1, N - 1),
    Map = lists:map(
        fun(X) ->
            if
                X rem 3 =:= 0; X rem 5 =:= 0 -> X;
                true -> 0
            end
        end,
        Seq
    ),
    lists:sum(Map).

% list comprehension
sum_multiples_comp(N) ->
    lists:sum([X || X <- lists:seq(1, N - 1), (X rem 3 =:= 0) orelse (X rem 5 =:= 0)]).
