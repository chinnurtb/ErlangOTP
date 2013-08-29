-module(chg3).
-export([change/1]).

change(X) ->
	BaseSum =  (X div 5) * 5,
	Delta = X - BaseSum,
	change(round(BaseSum, Delta), []).

round(X, Y) when Y >= 3 ->
	X + 5;
round(X, Y) ->
	X.

change(X, L) when X >= 200 ->
	change(X - 200, ['2.00' | L]);
change(X, L) when X >= 100 ->
	change(X - 100, ['1.00' | L]);
change(X, L) when X >= 50 ->
	change(X - 50, ['50' | L]);
change(X, L) when X >= 20 ->
	change(X - 20, ['20' | L]);
change(X, L) when X >= 10 ->
	change(X - 10, ['10' | L]);
change(X, L) when X >= 5 ->
	change(X - 5, ['5' | L]);
change(X, L) ->
	L.
