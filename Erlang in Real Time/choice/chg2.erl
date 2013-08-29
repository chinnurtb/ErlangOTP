-module(chg2).
-export([change/1]).

change(X) ->
	BaseSum =  (X div 5) * 5,
	Delta = X - BaseSum,
	case Delta of 
		3 -> Rounded = BaseSum + 5;
		4 -> Rounded = BaseSum + 5;
		_ -> Rounded = BaseSum
	end,
	change(Rounded, [200, 100, 50, 20, 10, 5], []).

change(X, VL, L) ->
	case {X, VL} of 
		{X, []} ->
			L;
		{X, [H|T]} when X >= H -> 
			change(X - H, VL, [toatom(H) | L]);
		{X, [H|T]} -> 
			change(X, T, L)
	end.

toatom(X) ->
	case X of 
		200 -> '2.00';
		100 -> '1.00';
		50 -> '50';
		20 -> '20';
		10 -> '10';
		5 -> '5'
	end.
