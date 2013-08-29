-module(tayser).
-export([exp/1, sin/1]).

epsilon() ->
	1.0e-8.

fact(0) -> 
	1;
fact(N) ->
	N * fact(N-1).

taylorterm(Z, N) ->
	math:pow(Z, N) / fact(N).

exp(Z) ->
	exp(Z, 0, epsilon()).

exp(Z, N, Epsilon) ->
	R = taylorterm(Z, N),
	if 
		R < Epsilon ->
			0;
		true ->
			R + exp(Z, N+1, Epsilon)
	end.

sin(Z) ->
	sin(Z, 0, 1, epsilon()).

sin(Z, N, 1, Epsilon) ->
	R = taylorterm(Z, (2*N)+1),
	if 
		R < Epsilon ->
			0;
		true ->
			R + sin(Z, N+1, -1, Epsilon)
	end;
sin(Z, N, -1, Epsilon) ->
	R = taylorterm(Z, (2*N)+1),
	if 
		R < Epsilon ->
			0;
		true ->
			-R + sin(Z, N+1, 1, Epsilon)
	end.
