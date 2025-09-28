-module(task30_tests).
-include_lib("eunit/include/eunit.hrl").

task30_test() ->
    Expected = 443839,
    ?assertEqual(Expected, task30:sum_powers_rec()),
    ?assertEqual(Expected, task30:sum_powers_tail()),
    ?assertEqual(Expected, task30:sum_powers_map()),
    ?assertEqual(Expected, task30:sum_powers_module()),
    ?assertEqual(Expected, task30:sum_powers_comp()).
