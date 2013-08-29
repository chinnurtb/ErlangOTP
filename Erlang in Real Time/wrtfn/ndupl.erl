-module(ndupl).
-export([ndupl/2]).

% ndupl(Item, N) - make a list containing N Items

ndupl(_, 0) ->
	[];
ndupl(Item, N) ->
	[Item | ndupl(Item, N-1)].
