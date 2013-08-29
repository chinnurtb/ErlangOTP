-module(time_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(TEST, test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([get_time/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  error_logger:info_msg("[~p] start_link()~n", [?MODULE]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_time() ->
  error_logger:info_msg("[~p] get_time()~n", [?MODULE]),
  gen_server:call(?SERVER, get_time).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  error_logger:info_msg("[~p] init(~p)~n", [?MODULE, Args]),
  {ok, Args}.

handle_call(Request, From, State) ->
  error_logger:info_msg("[~p] handle_call(~p,~p,~p)~n", [?MODULE, Request, From, State]),
  case Request of
    get_time ->
      Now = calendar:local_time(),
      NowSecs = calendar:datetime_to_gregorian_seconds(Now),
      {reply, {current_time, NowSecs}, State};
    _ ->
      {reply, {unrecognized, Request}, State}
  end.

handle_cast(Msg, State) ->
  error_logger:info_msg("[~p] handle_cast(~p,~p)~n", [?MODULE, Msg, State]),
  {noreply, State}.

handle_info(Info, State) ->
  error_logger:info_msg("[~p] handle_info(~p,~p)~n", [?MODULE, Info, State]),
  {noreply, State}.

terminate(Reason, State) ->
  error_logger:info_msg("[~p] terminate(~p,~p)~n", [?MODULE, Reason, State]),
  ok.

code_change(OldVsn, State, Extra) ->
  error_logger:info_msg("[~p] code_change(~p,~p,~p)~n", [?MODULE, OldVsn, State,Extra]),
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUnit Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

simple_test() ->
  ok = application:start(time_srv),
  ?assertNot(undefined == whereis(time_srv_sup)).

get_current_time_test() ->
  Before = calendar:local_time(),
  BeforeSecs = calendar:datetime_to_gregorian_seconds(Before),
  {current_time, ActualSecs} = time_srv:get_time(),
  After = calendar:local_time(),
  AfterSecs = calendar:datetime_to_gregorian_seconds(After),
  %assertions
  ?assert(BeforeSecs =< ActualSecs),
  ?assert(AfterSecs >= ActualSecs).

-endif.
