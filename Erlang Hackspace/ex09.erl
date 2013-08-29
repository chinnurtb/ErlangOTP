-module(ex09).
-export([start/0, loop/2, client/3]).

start() -> spawn(?MODULE, loop, [0,0]).

loop(Ops,Wtfs) ->
  receive
    {Client, double, Num} ->
      Client ! Num * 2,
      loop(Ops+1, Wtfs);

    {Client, triple, Num} ->
      Client ! Num * 3,
      loop(Ops+1, Wtfs);

    {Client, _, _Num} ->
      Client ! wtf,
      loop(Ops, Wtfs+1);

    reload ->
      io:format("Reloading~n"),
      ?MODULE:loop(Ops,Wtfs);
  
    stats ->
      io:format("Ops: ~p, Wtfs: ~p ~n", [Ops, Wtfs]),
      loop(Ops, Wtfs)
  end.

% basic client API:

client(Pid, Cmd, Num) ->
  Pid ! {self(), Cmd, Num},
  receive
    Ans -> Ans
  after 1000 -> timeout
  end.