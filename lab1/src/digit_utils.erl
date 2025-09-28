-module(digit_utils).
-author("nifreebie").

-export([sum_digit_pows/1]).

pow_list() ->
    [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049].

digits(0) -> [0];
digits(N) when N > 0 -> loop(N, []).

loop(0, Acc) ->
    Acc;
loop(N, Acc) ->
    D = N rem 10,
    loop(N div 10, [D | Acc]).

sum_digit_pows(N) ->
    Digits = digits(N),
    Pows = pow_list(),
    lists:foldl(fun(D, Acc) -> Acc + lists:nth(D + 1, Pows) end, 0, Digits).
