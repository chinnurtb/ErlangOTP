% RECURSION.
% If you don't understand recursion, ask the person next to you to explain it.


-module(ex04).
-export([list2x/1]).

list2x(List) -> list2x(List, []).

list2x([], Agg) -> Agg;

list2x([Head|Tail], Agg) -> list2x(Tail, [Head*2 | Agg]).

