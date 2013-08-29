-module(len2).
-export([len1/1, len2/1]).

len1([H|T]) ->
	1 + len1(T);
len1([]) ->
	0.

len2(L) ->
	len2(0, L).

len2(N, []) ->
	N;
len2(N, [H|T]) ->
	len2(N+1, T).
