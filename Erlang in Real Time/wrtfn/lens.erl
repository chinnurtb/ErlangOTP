-module(lens).
-export([len/1]).

len([]) ->
	0;
len(X) ->
	[H|T] = X,
	io:format("~w ~w~n", [H, T] ),
	LenT = len(T),
	io:format("~w ~w ~w ~w~n", [1 + LenT, H, LenT, T] ),
	1 + LenT.
