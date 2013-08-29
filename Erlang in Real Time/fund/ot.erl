-module(ot).
-export([ot/1]).

ot(X) when integer(X), X > 0 ->
	'positive natural';
ot(X) when integer(X), X < 0 ->
	'negative integer';
ot(X) when integer(X) ->
	integer;
ot(X) when float(X) ->
	float;
ot(X) when list(X) ->
	list;
ot(X) when tuple(X) ->
	tuple;
ot(X) when atom(X) ->
	atom.
