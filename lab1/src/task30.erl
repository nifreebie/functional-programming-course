-module(task30).
-author("nifreebie").

-import(digit_utils, [sum_digit_pows/1]).
-export([
    sum_powers_rec/0,
    sum_powers_tail/0,
    sum_powers_module/0,
    sum_powers_map/0,
    sum_powers_comp/0
]).

-define(LIMIT, 354294).

%% НЕ хвостовая рекурсия
sum_powers_rec() ->
    sum_powers_rec(?LIMIT).

sum_powers_rec(1) ->
    0;
sum_powers_rec(N) ->
    Rest = sum_powers_rec(N - 1),
    case sum_digit_pows(N) =:= N of
        true -> Rest + N;
        false -> Rest
    end.

% Xвостовая рекурсия
sum_powers_tail() ->
    loop(?LIMIT, 0).

loop(1, Acc) ->
    Acc;
loop(N, Acc) ->
    UpdateAcc =
        case sum_digit_pows(N) =:= N of
            true -> Acc + N;
            false -> Acc
        end,
    loop(N - 1, UpdateAcc).

% Модульная реализация
sum_powers_module() ->
    Seq = lists:seq(2, ?LIMIT),
    Ans = lists:filter(fun(N) -> sum_digit_pows(N) =:= N end, Seq),
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, Ans).

% Использование map
sum_powers_map() ->
    Seq = lists:seq(2, ?LIMIT),
    Map = lists:map(fun(N) -> {N, sum_digit_pows(N)} end, Seq),
    Ans = lists:filter(fun({N, S}) -> N =:= S end, Map),
    lists:foldl(fun({N, _}, Acc) -> N + Acc end, 0, Ans).

% list comprehension
sum_powers_comp() ->
    lists:sum([N || N <- lists:seq(2, ?LIMIT), sum_digit_pows(N) =:= N]).
