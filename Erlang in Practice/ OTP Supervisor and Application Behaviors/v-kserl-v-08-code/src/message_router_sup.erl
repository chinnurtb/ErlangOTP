-module(message_router_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  MessageRouter = {message_router, {message_router, start_link, []},
		   permanent, 5000, worker, [message_router]},
  MessageStore = {message_store, {message_store, start_link, []},
		  permanent, 5000, worker, [message_store]},
  {ok, {{one_for_all, 5, 30}, [MessageRouter, MessageStore]}}.
