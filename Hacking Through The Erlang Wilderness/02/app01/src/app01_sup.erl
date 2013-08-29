%%%----------------------------------------------------------------
%%% @author  Todd Greenwood-Geer <todd@niovb.com>
%%% @doc
%%% @end
%%% @copyright 2011 Todd Greenwood-Geer
%%%----------------------------------------------------------------
-module(app01_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    io:format("=========================================~n"),
    io:format("app01_sup:start_link()~n"),
    io:format("=========================================~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%% @private
-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                       ignore | {error, Reason::any()}.
init([]) ->
    io:format("=========================================~n"),
    io:format("app01_sup:init()~n"),
    io:format("=========================================~n"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    %AChild = {'AName', {'AModule', start_link, []},
    %          Restart, Shutdown, Type, ['AModule']},

    AChild = {app01, {app01, start_link, []},
              Restart, Shutdown, Type, [app01]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


