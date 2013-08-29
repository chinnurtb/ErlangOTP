-module(ex06).
-export([qs/1]).

qs([]) -> [];

qs([ Pivot | Tail ]) ->
  qs([ F || F <- Tail, F < Pivot ])
  ++ [Pivot] ++
  qs([ B || B <- Tail, B >= Pivot ]).

% use lists module tho.