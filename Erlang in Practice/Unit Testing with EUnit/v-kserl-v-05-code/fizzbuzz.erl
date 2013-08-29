-module(fizzbuzz).

-export([start/0, calc/2, generate_sequence/2, shutdown/1]).

start() ->
  spawn(fun() ->
	    loop() end).

calc(Pid, Value) ->
  Pid ! {analyze, self(), Value},
  receive
    Result ->
      Result
  end.

generate_sequence(Pid, N) ->
  case is_number(N) of
    false ->
      exit({badarg, N});
    true ->
      Pid ! {generate, self(), N},
      receive
	{ok, Result} ->
	  Result
      after 1000 ->
	  {error, timeout}
      end
  end.

shutdown(Pid) ->
  Pid ! shutdown.

analyze(N) ->
  if
    N rem 15 == 0 ->
      fizzbuzz;
    N rem 5 == 0 ->
      buzz;
    N rem 3 == 0 ->
      fizz;
    true ->
      N
  end.

loop() ->
  receive
    {analyze, Caller, Value} ->
      Caller ! analyze(Value),
      loop();
    {generate, Caller, Value} ->
      Caller ! {ok, generate_fizzbuzz_list(Value)},
      loop();
    shutdown ->
      io:format("Shutting down...~n"),
      ok
  end.

generate_fizzbuzz_list(N) ->
  generate_fizzbuzz_list(1, N, []).

generate_fizzbuzz_list(Pos, N, Accum) when Pos =< N ->
  generate_fizzbuzz_list(Pos + 1, N, lists:append(Accum, [analyze(Pos)]));
generate_fizzbuzz_list(Pos, N, Accum) when Pos > N->
  Accum.
