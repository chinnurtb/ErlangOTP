-module(ex03).
-export([ calc/1 ]).

calc(Op) ->
  case Op of
    {double, Num} -> Num+Num;
    {square, Num} -> Num*Num;
    {half,   Num} -> Num/2
  end.



%calc({double, Num}) -> Num+Num;
%
%calc({square, Num}) -> Num*Num;
%
%calc({half, Num}) -> Num/2;
%
%% idiomatic to only handle cases you expect, else crash
%%calc({_, Num}) -> Num.


