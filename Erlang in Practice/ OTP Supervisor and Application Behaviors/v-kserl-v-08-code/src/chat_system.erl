-module(chat_system).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
  mnesia:start(),
  application:start(?MODULE).

start(_Type, StartArgs) ->
  chat_system_sup:start_link(StartArgs).

stop(_State) ->
  ok.
