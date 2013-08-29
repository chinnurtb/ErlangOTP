-module(l2).
-export([flatten/1]).
-import(lists,[append/2]).

flatten(L) ->
	flatten([], L).

flatten(L, []) ->
	L;
flatten(L, [H|T]) when list(H) ->
	flatten(append(L, flatten(H)), T); 
flatten(L, [H|T]) ->
	flatten(append(L, [H]), T).
