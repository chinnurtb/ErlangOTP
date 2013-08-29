-module(web_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/1, init/1]).

start_link(Port) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  WebServer = {web_server, {web_server, start_link, [Port]},
	       permanent, 5000, worker, [web_server]},
  Mucc = {mucc, {mucc, start_link, []},
	  permanent, 5000, worker, [mucc]},
  {ok, {{one_for_one, 5, 30}, [WebServer, Mucc]}}.
