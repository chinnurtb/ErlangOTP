-module(drop_sup).
-behaviour(supervisor).
-export([start_link/0]). % convenience call for startup
-export([init/1]). % supervisor calls
-define(SERVER, ?MODULE). % macro that just defines this module as server


%%% convenience method for startup
start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% supervisor callback
init([]) ->
         RestartStrategy = one_for_one,
        MaxRestarts = 1, % one restart every
        MaxSecondsBetweenRestarts = 5, % five seconds

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Restart = permanent, % or temporary, or transient
        Shutdown = 2000, % milliseconds, could be infinity or brutal_kill
        Type = worker, % could also be supervisor

        Drop = {drop, {drop, start_link, []},
                          Restart, Shutdown, Type, [drop]},

        {ok, {SupFlags, [Drop]}}.


%%% Internal functions (none here)