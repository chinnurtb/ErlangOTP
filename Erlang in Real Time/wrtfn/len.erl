-module(len).
-export([len/1]).

len([]) ->
	0;
len([H | T]) ->
	1 + len(T).
