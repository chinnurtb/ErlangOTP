-module(test_fibber).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  {setup,
   fun() -> fibber:start() end,
   fun(Pid) -> fibber:shutdown(Pid) end,
   fun basic_generator/1}.

basic_generator(Pid) ->
  {timeout, 1, [?_assertEqual(1, fibber:calc(Pid, 0)),
		?_assertEqual(1, fibber:calc(Pid, 1)),
		?_assertEqual(1, fibber:calc(Pid, 2)),
		?_assertEqual(2, fibber:calc(Pid, 3))]}.
