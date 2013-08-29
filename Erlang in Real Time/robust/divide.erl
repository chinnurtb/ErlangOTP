-module(divide).
-export([divide/0]).

divide() ->
    X = (catch getarg()),
    case X of
        {'EXIT', Reason} ->
            io:format("Error ~w~n", [X]);
        {A, B} ->
            Y = (catch divide(A, B)),
            case Y of
                {'EXIT', Reason} ->
                    io:format("Error ~w~n", [Y]);
                Y -> Y
            end
    end.

getarg() ->
    {ok, [A, B]} = io:fread('Enter 2 numbers > ', "~d ~d"),
    {A, B}.

divide(A, B) ->
    D = A div B,
    io:format("~w~n", [D]).
