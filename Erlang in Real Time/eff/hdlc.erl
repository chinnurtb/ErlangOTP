-module(hdlc).
-export([enc/1,dec/1]).

delimit() ->
	[ 0, 1, 1, 1, 1, 1, 1, 0].

enc(L) ->
	X = enc(L, 0),
	lists:append(delimit(), lists:append(X, delimit())).

enc(L, 5) ->
	[0 | enc(L, 0)];
enc([H|T], N) ->
	if
		H == 1 ->
			[1 | enc(T, N+1)];
		H == 0 ->
			[H | enc(T, 0)]
	end;
enc([], _) ->
	[].

dec(L) -> 
	{Code, List} = dec(L, start, []),
	{Code, lists:reverse(List)}.

dec([0, 1, 1, 1, 1, 1, 1, 0 | T], start, _) -> 
	dec(T, message, []);
dec([H | T], start, _) -> 
	dec(T, start, []);
dec([], start, _) -> 
	{error, []};
dec([0, 1, 1, 1, 1, 1, 1, 0 | T], message, L) ->
	{ok, L};
dec([1, 1, 1, 1, 1, 1 | T], message, L) ->
	{error, L};
dec([1, 1, 1, 1, 1, 0 | T], message, L) ->
	dec(T, message, [1, 1, 1, 1, 1 | L]);
dec([H|T], message, L) ->
	dec(T, message, [H | L]);
dec([], message, L) ->
	{error, L}.
