-module(cntdwn).
-export([start/0]).

start() ->
	cntdwn(10).

cntdwn(N) when N > 0 ->
	io:format("~w~n", [N]),
	receive
	after 1000 -> 
		true
	end,
	cntdwn(N-1);
cntdwn(_) ->
	io:format("ZERO~n").
