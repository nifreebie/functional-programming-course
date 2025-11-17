-module(reader).
-export([start/1]).

start(CoordPid) ->
    loop(CoordPid).

loop(CoordPid) ->
    case io:get_line("") of
        eof ->
            CoordPid ! {eof, self()},
            ok;
        Line ->
            Str = string:trim(Line),
            case Str of
                "" ->
                    loop(CoordPid);
                _ ->
                    case parse_pair(Str) of
                        {ok, {X, Y}} ->
                            CoordPid ! {point, {X, Y}},
                            loop(CoordPid);
                        {error, Reason} ->
                            io:format(stderr, "reader: cannot parse line '~s': ~p~n", [Line, Reason]),
                            loop(CoordPid)
                    end
            end
    end.

parse_pair(Line) ->
    Tokens = string:tokens(Line, " \t,;"),
    case Tokens of
        [Xs, Ys] ->
            try
                X = list_to_float(Xs),
                Y = list_to_float(Ys),
                {ok, {X, Y}}
            catch
                _:Err -> {error, Err}
            end;
        _ ->
            {error, bad_format}
    end.
