-module(time_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  error_logger:info_msg("[~p] init(~p)~n", [?MODULE, Args]),
  {ok, Args}.

handle_call(Request, From, State) ->
  error_logger:info_msg("[~p] handle_call(~p,~p,~p)~n", [?MODULE, Request, From, State]),
  {noreply, ok, State}.

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

