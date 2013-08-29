-module(coderep).
-export([msglp/0]).

vers() ->
	1.

msglp() ->
	Msg = receive
		X -> X
	end,
	io:format("~w ~w~n", [Msg, vers()]),
	coderep:msglp().
