-module(divide2a).
-export([divide/0]).

divide() ->
    io:format("Enter  numbers "),
    {R1, A} = readint(),
    {R2, B} = readint(),
    if
        R1 == ok, R2 == ok ->
            if
                B =/= 0 ->
                    D = A div B,
                    io:format("~w~n", [D]),
                    D;
                true ->
                    io:format("Attempt to divide by zero~n"),
                    divide()
            end;
        true ->
            io:format("Please enter 2 numbers~n"),
            divide()
    end.

readint() ->
    io:format("> "),
    {ok, [L]} = io:fread('', "~s"),
    Len = string:span(L, "0123456789"),
    if
        Len == 0 ->
            {nodata, 0};
        true ->
            V = list_to_integer(string:substr(L,1,Len)),
            {ok, V}
    end.
