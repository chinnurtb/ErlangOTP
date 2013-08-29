-module(app).
-export([app/2]).

app([], L) ->
	L;
app([H|T], L) ->
	[H | app(T, L)].
