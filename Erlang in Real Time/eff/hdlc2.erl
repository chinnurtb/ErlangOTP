-module(hdlc2).
-export([enc/1,dec/1]).

delimit() ->
	[ 0, 1, 1, 1, 1, 1, 1, 0].

enc(L) ->
	X = enc(0, L),
	lists:append(delimit(), lists:append(X, delimit())).

enc(5, L) ->
	[0 | enc(0, L)];
enc(N, [1|T]) ->
	[1 | enc(N+1, T)];
enc(N, [0|T]) ->
	[0 | enc(0, T)];
enc(_, []) ->
	[].

dec(L) -> 
	{Code, List} = dec(start, L, []),
	{Code, lists:reverse(List)}.

dec(start, [0, 1, 1, 1, 1, 1, 1, 0 | T], _) -> 
	dec(message, T, []);
dec(start, [H | T], _) -> 
	dec(start, T, []);
dec(start, [], _) -> 
	{error, []};
dec(message, [0, 1, 1, 1, 1, 1, 1, 0 | T], L) ->
	{ok, L};
dec(message, [1, 1, 1, 1, 1, 1 | T], L) ->
	{error, L};
dec(message, [1, 1, 1, 1, 1, 0 | T], L) ->
	dec(message, T, [1, 1, 1, 1, 1 | L]);
dec(message, [H|T], L) ->
	dec(message, T, [H | L]);
dec(message, [], L) ->
	{error, L}.
