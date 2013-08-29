-module(divide2b).
-export([divide/0]).

divide() ->
    case (catch getdiv()) of
        {'EXIT', Reason} ->
            io:format("Error ~w~n", [Reason]),
            divide();
        X -> X
    end.

getdiv() ->
    {ok, [A, B]} = io:fread('Enter 2 numbers > ', "~d ~d"),
    D = A div B,
    io:format("~w~n", [D]).
