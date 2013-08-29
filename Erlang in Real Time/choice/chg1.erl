-module(chg1).
-export([change/1]).

change(X) ->
	BaseSum =  (X div 5) * 5,
	Delta = X - BaseSum,
	if 
		Delta >= 3 -> change(BaseSum + 5, []);
		true -> change(BaseSum, [])
	end.

change(X, L) ->
	if 
		X >= 200 -> change(X - 200, ['2.00' | L]);
		X >= 100 -> change(X - 100, ['1.00' | L]);
		X >=  50 -> change(X -  50, ['50' | L]);
		X >=  20 -> change(X -  20, ['20' | L]);
		X >=  10 -> change(X -  10, ['10' | L]);
		X >=   5 -> change(X -   5, ['5' | L]);
		true -> L
	end.
