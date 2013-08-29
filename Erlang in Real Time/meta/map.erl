-module(map).
-export([map/2]).

map(L, Fn) ->
	map(L, [], Fn).

map([], N, Fn) ->
	N;
map([H|T], N, Fn) ->
	map(T, [apply(Fn, [H]) | N], Fn).
