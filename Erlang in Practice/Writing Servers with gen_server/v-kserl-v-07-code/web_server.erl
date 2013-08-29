-module(web_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(OK, <<"ok">>).

%% API
-export([start_link/1, dispatch_requests/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([Port]) ->
  mochiweb_http:start([{port, Port},
		       {loop, fun(Req) ->
				  dispatch_requests(Req) end}]),
  erlang:monitor(process, mochiweb_http),
  {ok, []}.

stop() ->
  gen_server:cast(?SERVER, stop).

dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _, _, {mochiweb_http, _}, _}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  mochiweb_http:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle("/send", Req) ->
  Params = Req:parse_qs(),
  Sender = proplists:get_value("nick", Params),
  Addressee = proplists:get_value("to", Params),
  Message = proplists:get_value("msg", Params),
  mucc:send_message(Sender, Addressee, Message),
  success(Req, ?OK);

handle("/poll", Req) ->
  Params = Req:parse_qs(),
  Nickname = proplists:get_value("nick", Params),
  case mucc:poll(Nickname) of
    {error, Error} ->
      error(Req, subst("Error: ~s~n", Error));
    Messages ->
      case length(Messages) == 0 of
	true ->
	  success(Req, <<"none">>);
	false ->
	  Template = lists:foldl(fun(_, Acc) -> ["~s~n"|Acc] end, [], Messages),
	  success(Req, subst(lists:flatten(Template), Messages))
      end
  end;

handle("/unregister", Req) ->
  Params = Req:parse_qs(),
  Nickname = proplists:get_value("nick", Params),
  mucc:unregister(Nickname),
  success(Req, ?OK);

handle("/register", Req) ->
  Params = Req:parse_qs(),
  Nickname = proplists:get_value("nick", Params),
  case mucc:register_nickname(Nickname) of
    ok ->
      success(Req, ?OK);
    Error ->
      error(Req, subst("Error: ~s", [Error]))
  end;
handle(_, Req) ->
  error(Req, "").

error(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Body}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.
