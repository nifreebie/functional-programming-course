-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").

insert_sorted_empty_test() ->
    ?assertEqual([{0.0, 0.0}], utils:insert_sorted([], {0.0, 0.0})).

insert_sorted_begin_test() ->
    ?assertEqual([{0.0, 0.0}, {1.0, 1.0}], utils:insert_sorted([{1.0, 1.0}], {0.0, 0.0})).

insert_sorted_middle_test() ->
    ?assertEqual(
        [{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}],
        utils:insert_sorted([{0.0, 0.0}, {2.0, 2.0}], {1.0, 1.0})
    ).

insert_sorted_replace_test() ->
    ?assertEqual(
        [{1.0, 9.0}, {2.0, 2.0}],
        utils:insert_sorted([{1.0, 1.0}, {2.0, 2.0}], {1.0, 9.0})
    ).
