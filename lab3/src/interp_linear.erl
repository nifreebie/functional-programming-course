-module(interp_linear).
-export([interp/3]).

%% interp(Buffer, OptsMap, X) -> {ok, Y} | {error, Reason}
interp(Buffer, _Opts, X) ->
    case find_segment(Buffer, X) of
        {ok, {X0, Y0}, {X1, Y1}} ->
            %% if exact match
            if
                X1 =:= X0 ->
                    {ok, Y0};
                X =:= X0 ->
                    {ok, Y0};
                X =:= X1 ->
                    {ok, Y1};
                true ->
                    T = (X - X0) / (X1 - X0),
                    Y = Y0 + T * (Y1 - Y0),
                    {ok, Y}
            end;
        {error, no_segment} ->
            {error, no_data}
    end.

find_segment([], _X) ->
    {error, no_segment};
find_segment([P], X) ->
    case P of
        {Xp, Yp} ->
            if
                X =:= Xp -> {ok, {Xp, Yp}, {Xp, Yp}};
                true -> {error, no_segment}
            end
    end;
find_segment([P1, P2 | Rest], X) ->
    {X1, Y1} = P1,
    {X2, Y2} = P2,
    if
        X >= X1 andalso X =< X2 ->
            {ok, P1, P2};
        true ->
            find_segment([P2 | Rest], X)
    end.
