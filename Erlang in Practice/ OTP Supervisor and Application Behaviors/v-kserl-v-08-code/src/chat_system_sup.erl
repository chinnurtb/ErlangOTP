-module(chat_system_sup).

-define(SERVER, ?MODULE).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
  MessageRouterSup = {message_router_sup, {message_router_sup, start_link, []},
		      permanent, infinity, supervisor, [message_router_sup]},
  WebSup = {web_sup, {web_sup, start_link, [Port]},
	    permanent, infinity, supervisor, [web_sup]},
  {ok, {{one_for_one, 5, 60}, [MessageRouterSup, WebSup]}}.
