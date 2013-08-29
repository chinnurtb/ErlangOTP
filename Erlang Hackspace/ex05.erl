-module(ex05).
-export([listfun/2]).

listfun(Fun, List) -> listfun(Fun, List, []).

listfun(_, [], Agg) -> Agg;

listfun(Fun, [Head|Tail], Agg) -> 
  listfun(Fun, Tail, [ Fun(Head) | Agg ]).

% but, use list comprehensions and lists module!