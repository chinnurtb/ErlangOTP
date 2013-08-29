-module(maxmin).
-export([max/2,min/2]).

max(A,B) ->
	if 
		A > B -> A;
		true -> B
	end.

min(A,B) ->
	if 
		A < B -> A;
		true -> B
	end.
