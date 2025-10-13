-module(prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    prop_union_associative/0,
    prop_union_identity/0,
    prop_map_identity/0,
    prop_runner_test/0
]).

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

prop_runner_test() ->
    Opts = [{numtests, 3}, {max_size, 20}, {timeout, 2000}],
    Failed = proper:module(?MODULE, Opts),
    ?_assertEqual([], Failed).
