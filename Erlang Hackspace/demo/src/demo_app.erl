%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the demo application.

-module(demo_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for demo.
start(_Type, _StartArgs) ->
    demo_deps:ensure(),
    demo_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for demo.
stop(_State) ->
    ok.
