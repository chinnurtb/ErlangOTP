-module(mtocon).
-export([start/1]).

start(N) ->
    register(N, self()),
    io:format("Starting ~w~n", [N]),
    loop(N).

loop(N) ->
    receive
        {exit, Reason} ->
            io:format("Exiting ~w because ~w~n", [N, Reason]),
            exit(Reason);
        X -> 
            io:format("~w received ~w", [N, X])
    end,
    loop(N).
