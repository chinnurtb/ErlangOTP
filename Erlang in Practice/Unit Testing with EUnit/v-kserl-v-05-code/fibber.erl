-module(fibber).

-export([start/0, shutdown/1, calc/2]).

start() ->
  spawn(fun() -> loop() end).

shutdown(Pid) ->
  Pid ! shutdown.

calc(Pid, N) ->
  Pid ! {fib, self(), N},
  receive
    {ok, Value} ->
      Value
  after 5000 ->
      {error, timeout}
  end.

loop() ->
  receive
    {fib, Caller, N} ->
      Caller ! {ok, fib(N)},
      loop();
    shutdown ->
      ok
  end.

fib(0) ->
  0;
fib(1) ->
  1;
fib(2) ->
  1;
fib(N) ->
  fib(N - 2) + fib(N - 1).
