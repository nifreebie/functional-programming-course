-module(task1_tests).
-include_lib("eunit/include/eunit.hrl").

task1_test() ->
    Expected = 233168,
    ?assertEqual(Expected, task1:sum_multiples_rec(1000)),
    ?assertEqual(Expected, task1:sum_multiples_tail(1000)),
    ?assertEqual(Expected, task1:sum_multiples_map(1000)),
    ?assertEqual(Expected, task1:sum_multiples_module(1000)),
    ?assertEqual(Expected, task1:sum_multiples_comp(1000)).
