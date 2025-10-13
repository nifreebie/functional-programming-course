-module(unit_tests).
-include_lib("eunit/include/eunit.hrl").

add_remove_test() ->
    S = oaset:new(),
    S1 = oaset:add(1, S),
    ?assert(oaset:contains(S1, 1)),
    ?assert(oaset:size(S1) =:= 1),
    S2 = oaset:remove(1, S1),
    ?assert(not oaset:contains(S2, 1)),
    ?assert(oaset:size(S2) =:= 0).

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
