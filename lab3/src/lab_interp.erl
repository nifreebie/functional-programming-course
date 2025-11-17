-module(lab_interp).
-export([main/1, parse_args/1]).
-record(opts, {algorithms = [], step = 1.0, newton_n = 4}).
-include_lib("kernel/include/logger.hrl").

main(Args) ->
    Opts = parse_args(Args),
    Printer = spawn(printer, start, []),
    Algs = build_algorithms(Opts),
    Coord = spawn(coordinator, start, [Algs, {step, Opts#opts.step}, Printer, Opts]),
    _Reader = spawn(reader, start, [Coord]),
    wait_for_exit().

parse_args(Args) ->
    Rec0 = #opts{},
    parse_args(Args, Rec0).

parse_args([], Rec) ->
    Rec;
parse_args(["--alg", A | Rest], Rec) ->
    Names = string:tokens(A, ","),
    parse_args(Rest, Rec#opts{algorithms = Names});
parse_args(["--step", S | Rest], Rec) ->
    Step = list_to_float(S),
    parse_args(Rest, Rec#opts{step = Step});
parse_args(["--newton-n", N | Rest], Rec) ->
    parse_args(Rest, Rec#opts{newton_n = list_to_integer(N)});
parse_args([_Unknown | Rest], Rec) ->
    parse_args(Rest, Rec).

build_algorithms(Opts) ->
    Names = Opts#opts.algorithms,
    Mapped = lists:map(
        fun(Name) ->
            case string:lowercase(Name) of
                "linear" ->
                    {linear, interp_linear, #{name => "linear"}};
                "newton" ->
                    {newton, interp_newton, #{name => "newton", n => Opts#opts.newton_n}};
                _Other ->
                    io:format(stderr, "Unknown algorithm ~p, skipping~n", [Name]),
                    undefined
            end
        end,
        Names
    ),
    lists:filter(fun(X) -> X =/= undefined end, Mapped).

wait_for_exit() ->
    receive
        stop -> ok
    after 1000 ->
        wait_for_exit()
    end.
