%%%----------------------------------------------------------------
%%% @author Todd Greenwood-Geer <todd@niovb.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2011 Todd Greenwood-Geer
%%%----------------------------------------------------------------,
-module(time_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
-spec start(normal | {takeover, node()} | {failover, node()},
            any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                      {error, Reason::any()}.
start(StartType, StartArgs) ->
  error_logger:info_msg("[~p] start(~p,~p)~n", [?MODULE, StartType, StartArgs]),
  case time_srv_sup:start_link() of
      {ok, Pid} ->
          {ok, Pid};
      Error ->
          Error
  end.

%% @private
-spec stop(State::any()) -> ok.
stop(State) ->
  error_logger:info_msg("[~p] stop(~p)~n", [?MODULE, State]),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


