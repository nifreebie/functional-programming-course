-module(utils).
-export([insert_sorted/2]).

insert_sorted([], P) ->
    [P];
insert_sorted([H | T] = List, P = {X, Y}) ->
    {HX, _HY} = H,
    case X =:= HX of
        true -> [P | T];
        false when X < HX -> [P | List];
        false -> [H | insert_sorted(T, P)]
    end.
