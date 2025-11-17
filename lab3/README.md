# Лабораторная работа №3 

- Студент: `Исупов Никита Александрович`
- Группа: `P3331`
- Язык: `Erlang`
---

## Ключевые элементы реализации

- **Архитектура на процессах:**
  - `reader` — читает `stdin` построчно, парсит пары чисел и шлёт координаты координатору.
    ```erlang
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

    ```
  - `interp_linear` — линейная интерополяция.
    ```erlang
    -module(interp_linear).
    -export([interp/3]).

    interp(Buffer, _Opts, X) ->
        case find_segment(Buffer, X) of
            {ok, {X0, Y0}, {X1, Y1}} ->
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
    ```
  - `interp_newton` — интерполяция Ньютна.
    ```erlang
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
    ```
  - `printer` — поток вывода в `stdout`.
    ```erlang
    -module(printer).
    -export([start/0]).

    start() ->
        loop().

    loop() ->
        receive
            {output, AlgName, X, Y} ->
                io:format("~s: ~p ~p~n", [AlgName, X, Y]),
                loop();
            {finished} ->
                io:format("~n--- finished ---~n"),
                loop();
            {done} ->
                io:format("~n--- done (EOF received) ---~n"),
                %% exit
                ok;
            Other ->
                io:format("printer: unknown ~p~n", [Other]),
                loop()
        end.
    ```

  - `coordinator` — держит буфер точек, генерирует запросы для сетки step, вызывает алгоритмы и посылает результаты в printer.
    ```erlang
    -module(coordinator).
    -export([start/4]).

    -define(DEFAULT_BUF_CAP, 10000).

    start(Algs, {step, Step}, PrinterPid, _Opts) ->
        State = #{
            algs => Algs,
            buffer => [],
            next_x => undefined,
            step => Step,
            max_x => undefined,
            printer => PrinterPid,
            eof => false
        },
        loop(State).

    loop(State) ->
        receive
            {point, {X, Y}} ->
                Buffer0 = maps:get(buffer, State),
                NewBuf = utils:insert_sorted(Buffer0, {X, Y}),

                MaxX =
                    case maps:get(max_x, State) of
                        undefined ->
                            X;
                        OldMax ->
                            if
                                X > OldMax -> X;
                                true -> OldMax
                            end
                    end,

                NextX =
                    case maps:get(next_x, State) of
                        undefined -> X;
                        V -> V
                    end,

                NewState = State#{
                    buffer := NewBuf,
                    max_x := MaxX,
                    next_x := NextX
                },

                process_ready_outputs(NewState);
            {eof, _From} ->
                NewState = State#{eof := true},
                process_ready_outputs(NewState),
                Printer = maps:get(printer, State),
                Printer ! {done},
                exit(normal);
            _Other ->
                loop(State)
        end.

    process_ready_outputs(State) ->
        Buffer = maps:get(buffer, State),
        case Buffer of
            [] ->
                loop(State);
            _ ->
                Step = maps:get(step, State),
                NextX = maps:get(next_x, State),
                MaxX = maps:get(max_x, State),
                
                NextX1 =
                    case NextX of
                        undefined ->
                            {X0, _Y0} = hd(Buffer),
                            X0;
                        V ->
                            V
                    end,

                process_loop(State#{next_x := NextX1})
        end.

    process_loop(State) ->
        NextX = maps:get(next_x, State),
        MaxX = maps:get(max_x, State),
        Step = maps:get(step, State),
        Algs = maps:get(algs, State),
        Printer = maps:get(printer, State),
        Buffer = maps:get(buffer, State),
        EOF = maps:get(eof, State),

        if
            NextX =< MaxX ->
                lists:foreach(
                    fun({Id, Module, Mopts}) ->
                        case Module:interp(Buffer, Mopts, NextX) of
                            {ok, Y} ->
                                Printer ! {output, maps:get(name, Mopts), NextX, Y};
                            {error, _} ->
                                ok
                        end
                    end,
                    Algs
                ),

                NewNext = float_add(NextX, Step),
                process_loop(State#{next_x := NewNext});
            EOF == true ->
                Printer ! {finished},
                loop(State);
            true ->
                loop(State)
        end.

    float_add(A, B) ->
        A + B.
    ```
  - `lab_interp` — точка входа в приложение.
    ```erlang
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
    ```

- **Интерполяционные методы:**
  - Линейная: вычисление по отрезкам через угловой коэффициент.
  - Ньютон: разделённые разности, построение полинома n-й степени.

- **Управление через CLI:**
  ```
  usage:
    lab_interp [--alg <linear> <newton>]
               [--step <h>] [-n <k>] [--precision 6]
  ```


## Ввод и вывод программы

**Пример ввода (`stdin`):**

```
# x y 
0.0 0.0
1.0 1.0
2.0 2.0
3.0 3.0
```

**Пример вывода (`stdout`):**

`% cat data.txt | ./lab_interp  --alg linear --step 0.7`


```
linear: 0.0 0.0
linear: 0.7 0.7
linear: 1.4 1.4
linear: 2.0999999999999996 2.0999999999999996
linear: 2.8 2.8
```

## Тесты

```erlang
basic_insert_test() ->
    Buf0 = [],
    Buf1 = utils:insert_sorted(Buf0, {1, 1}),
    Buf2 = utils:insert_sorted(Buf1, {0, 0}),
    ?assertEqual([{0, 0}, {1, 1}], Buf2).

interp_linear_exact_point_test() ->
    ?assertEqual({ok, 0.0}, interp_linear:interp(buf(), #{}, 0.0)),
    ?assertEqual({ok, 1.0}, interp_linear:interp(buf(), #{}, 1.0)),
    ?assertEqual({ok, 2.0}, interp_linear:interp(buf(), #{}, 2.0)).

interp_linear_midpoint_test() ->
    {ok, Y} = interp_linear:interp(buf(), #{}, 0.5),
    ?assertEqual(0.5, Y).

interp_linear_between_test() ->
    ?assertEqual({ok, 1.4}, interp_linear:interp(buf(), #{}, 1.4)).

buf4() -> [{0.0,0.0},{1.0,1.0},{2.0,2.0},{3.0,3.0}].

interp_newton_basic_test() ->
    Opt = #{name => "newton", n => 4},
    ?assertEqual({ok,0.0}, interp_newton:interp(buf4(), Opt, 0.0)),
    ?assertEqual({ok,1.5}, interp_newton:interp(buf4(), Opt, 1.5)),
    ?assertEqual({ok,3.0}, interp_newton:interp(buf4(), Opt, 3.0)).

interp_newton_exact_test() ->
    Opt = #{name => "newton", n => 4},
    ?assertEqual({ok,2.0}, interp_newton:interp(buf4(), Opt, 2.0)).

parse_args_test() ->
    {opts, ["linear"], 0.7, 4} =
        lab_interp:parse_args(["--alg", "linear", "--step", "0.7", "--n", "4"]).


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
```

## Выводы

В работе реализована потоковая архитектура на Erlang, где каждая часть — отдельный процесс:

- Чтение, вычисление и вывод изолированы, что упрощает тестирование и масштабирование.
- Использование механизма обмена сообщениями (! и receive) позволило изолировать процессы и упростить параллельную обработку входного потока. Такой подход обеспечивает высокую устойчивость и масштабируемость системы, однако требует аккуратного проектирования протоколов сообщений, поскольку селективное получение может приводить к зависанию нежелательных сообщений. 