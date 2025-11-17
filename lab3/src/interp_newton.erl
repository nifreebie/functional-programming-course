-module(interp_newton).
-export([interp/3]).

interp(Buffer, Mopts, X) ->
    N = maps:get(n, Mopts, 4),
    case find_window(Buffer, N, X) of
        {ok, Window} ->
            case find_exact(Window, X) of
                {ok, Y} ->
                    {ok, Y};
                not_found ->
                    Xs = [X0 || {X0, _} <- Window],
                    Ys = [Y0 || {_, Y0} <- Window],
                    Coefs = compute_coeffs(Xs, Ys),
                    Y = eval_newton(Coefs, Xs, X),
                    {ok, Y}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

find_window(Buffer, N, X) ->
    Len = length(Buffer),
    if
        Len < N -> {error, not_enough_points};
        true -> find_window_i(Buffer, N, X, 1, Len)
    end.

find_window_i(_Buf, _N, _X, I, Len) when I > Len -> {error, no_window};
find_window_i(Buf, N, X, I, Len) ->
    To = I + N - 1,
    case slice(Buf, I, To) of
        Segment when length(Segment) =:= N ->
            {X1, _} = hd(Segment),
            {Xn, _} = lists:last(Segment),
            if
                X >= X1 andalso X =< Xn -> {ok, Segment};
                true -> find_window_i(Buf, N, X, I + 1, Len)
            end;
        _ ->
            {error, no_window}
    end.

find_exact(Window, X) ->
    case lists:filter(fun({Xi, _}) -> Xi =:= X end, Window) of
        [{_, Y}] -> {ok, Y};
        _ -> not_found
    end.

slice(List, From, To) ->
    slice_i(List, 1, From, To, []).

slice_i([], _Pos, _From, _To, Acc) ->
    lists:reverse(Acc);
slice_i(_List, Pos, _From, To, Acc) when Pos > To -> lists:reverse(Acc);
slice_i([H | T], Pos, From, To, Acc) ->
    NewAcc =
        if
            Pos >= From andalso Pos =< To -> [H | Acc];
            true -> Acc
        end,
    slice_i(T, Pos + 1, From, To, NewAcc).

compute_coeffs(Xs, Ys) ->
    compute_coeffs_loop(Xs, Ys, 0, []).

compute_coeffs_loop(_Xs, Ys, _K, Acc) when Ys == [] ->
    lists:reverse(Acc);
compute_coeffs_loop(_Xs, Ys, _K, Acc) when length(Ys) == 1 ->
    lists:reverse([hd(Ys) | Acc]);
compute_coeffs_loop(Xs, Ys, K, Acc) ->
    Coeff = hd(Ys),
    NextYs = next_diffs_k(Ys, Xs, K),
    compute_coeffs_loop(Xs, NextYs, K + 1, [Coeff | Acc]).

next_diffs_k(Ys, Xs, K) ->
    next_diffs_k_i(Ys, Xs, K, 1, []).

next_diffs_k_i(Ys, Xs, K, I, Acc) ->
    Len = length(Ys),
    if
        I >= Len ->
            lists:reverse(Acc);
        true ->
            A = lists:nth(I, Ys),
            B = lists:nth(I + 1, Ys),
            X1 = lists:nth(I, Xs),
            X2 = lists:nth(I + K + 1, Xs),
            V = (B - A) / (X2 - X1),
            next_diffs_k_i(Ys, Xs, K, I + 1, [V | Acc])
    end.

eval_newton(Coefs, Xs, X) ->
    N = length(Coefs),
    Eval0 = lists:nth(N, Coefs),
    eval_newton_loop(Coefs, Xs, X, N - 1, Eval0).

eval_newton_loop(_Coefs, _Xs, _X, 0, Acc) ->
    Acc;
eval_newton_loop(Coefs, Xs, X, K, Acc) ->
    A = lists:nth(K, Coefs),
    Xi = lists:nth(K, Xs),
    NewAcc = A + (X - Xi) * Acc,
    eval_newton_loop(Coefs, Xs, X, K - 1, NewAcc).
