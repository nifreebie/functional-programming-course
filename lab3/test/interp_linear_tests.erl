-module(interp_linear_tests).
-include_lib("eunit/include/eunit.hrl").

buf() -> [{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}].

interp_linear_exact_point_test() ->
    ?assertEqual({ok, 0.0}, interp_linear:interp(buf(), #{}, 0.0)),
    ?assertEqual({ok, 1.0}, interp_linear:interp(buf(), #{}, 1.0)),
    ?assertEqual({ok, 2.0}, interp_linear:interp(buf(), #{}, 2.0)).

interp_linear_midpoint_test() ->
    {ok, Y} = interp_linear:interp(buf(), #{}, 0.5),
    ?assertEqual(0.5, Y).

interp_linear_between_test() ->
    ?assertEqual({ok, 1.4}, interp_linear:interp(buf(), #{}, 1.4)).
