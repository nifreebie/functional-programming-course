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
