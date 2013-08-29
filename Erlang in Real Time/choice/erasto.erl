-module(erasto).
-export([era/1]).

era(N) ->
	[1 | era(math:sqrt(N), fill(2, N))].

fill(L, L) ->
	[L];
fill(L, H) when H > L ->
	[L | fill(L+1, H)].

era(Max, L) when hd(L) =< Max ->
	Prime = hd(L),
	[Prime | era(Max, sieve(Prime, L))];
era(Max, L) -> 
	L.

sieve(N, []) ->
	[];
sieve(N, [H|T]) when H rem N == 0 ->
	sieve(N, T);
sieve(N, [H|T]) ->
	[H | sieve(N, T)].
