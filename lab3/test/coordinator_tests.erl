-module(coordinator_tests).
-include_lib("eunit/include/eunit.hrl").

basic_insert_test() ->
    Buf0 = [],
    Buf1 = utils:insert_sorted(Buf0, {1, 1}),
    Buf2 = utils:insert_sorted(Buf1, {0, 0}),
    ?assertEqual([{0, 0}, {1, 1}], Buf2).
