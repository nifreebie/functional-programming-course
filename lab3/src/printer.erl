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
