-module(hex).
-export([hexdec/1, hexadd/0]).

hexadd() ->
    X = (catch adder()),
    case X of
        {'EXIT', Reason} ->
            io:format("Error ~w~n", [X]);
        {fail, badchar} ->
            io:format("Error hex digits must are 0-9 a-f~n");
        {fail, nullarg} ->
            io:format("Error value required~n");
        Y -> Y
    end.

adder() ->
    {ok, [A]} = io:fread('Enter first number > ', "~a"),
    {ok, [B]} = io:fread('Enter second number > ', "~a"),
    hexdec(A) + hexdec(B).

hexdec(Atom) ->
    L = atom_to_list(Atom),
    case (catch hexcvt(L)) of
        {'EXIT', {function_clause, _}} ->
            throw({fail, badchar});
        {fail, X} ->
            throw({fail, X});
        R -> R
    end.

hexcvt([]) ->
    throw({fail, nullarg});
hexcvt(L) ->
    hexcvt(L, 0).

hexcvt([], N) ->
    N;
hexcvt([H|T], N) ->
    V = decval(H),
    hexcvt(T, N * 16 + V).

decval($0) -> 0;  decval($1) -> 1;  decval($2) -> 2;
decval($3) -> 3;  decval($4) -> 4;  decval($5) -> 5;
decval($6) -> 6;  decval($7) -> 7;  decval($8) -> 8;
decval($9) -> 9;  decval($a) -> 10; decval($b) -> 11;
decval($c) -> 12; decval($d) -> 13; decval($e) -> 14;
decval($f) -> 15.
