-module(lab4_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    p2p_node:start_link([{port, 5000}]).

stop(_State) ->
    ok.
