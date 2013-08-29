-module(ex07).
-export([start/0]).

start() -> spawn(fun loop/0).

loop() ->
  receive
    ping ->
      io:format("Got a ping!~n");
    X ->
      io:format("Got something else: ~p~n", [X])
  end,
  loop().