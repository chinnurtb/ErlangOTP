%%%----------------------------------------------------------------
%%% @author Todd Greenwood-Geer <todd@niovb.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2011 Todd Greenwood-Geer
%%%----------------------------------------------------------------,
-module(app01_app).

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
start(_StartType, _StartArgs) ->
    io:format("=========================================~n"),
    io:format("app01_app: start()~n"),
    io:format("=========================================~n"),
    case app01_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    io:format("=========================================~n"),
    io:format("app01_app: stop()~n"),
    io:format("=========================================~n"),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


