%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(demo).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the demo server.
start() ->
    demo_deps:ensure(),
    ensure_started(crypto),
    application:start(demo).

%% @spec stop() -> ok
%% @doc Stop the demo server.
stop() ->
    Res = application:stop(demo),
    application:stop(crypto),
    Res.
