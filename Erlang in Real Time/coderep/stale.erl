-module(stale).
-export([start/0, stale/0, vers/0]).

vers() ->
    1.

start() ->
    spawn(stale, stale, []).

stale() ->
    receive
        startver -> 
            io:format("Start Version ~w~n", [vers()]);
        curver -> 
            io:format("Current Version ~w~n", [stale:vers()])
    end,
    stale().
