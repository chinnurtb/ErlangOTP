-module(ex08).
-export([start/0, loop/2, r/0]).

start() -> spawn(ex08, loop, [0,0]).

loop(Ops,Wtfs) ->
  receive
    {Client, double, Num} ->
      Client ! Num * 2,
      loop(Ops+1, Wtfs);

    {Client, square, Num} ->
      Client ! Num * Num,
      loop(Ops+1, Wtfs);

    {Client, _, _Num} ->
      Client ! wtf,
      loop(Ops, Wtfs+1);

    stats ->
      io:format("Ops: ~p, Wtfs: ~p ~n", [Ops, Wtfs]),
      loop(Ops, Wtfs)
  end.


r() ->
  receive 
    X -> X
  after 1000 -> timeout
  end.
