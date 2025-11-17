-module(lab_interp_tests).
-include_lib("eunit/include/eunit.hrl").

parse_args_test() ->
    {opts, ["linear"], 0.7, 4} =
        lab_interp:parse_args(["--alg", "linear", "--step", "0.7", "--n", "4"]).
