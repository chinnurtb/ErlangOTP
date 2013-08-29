%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the mywebdemo application.

-module(mywebdemo_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mywebdemo.
start(_Type, _StartArgs) ->
    mywebdemo_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mywebdemo.
stop(_State) ->
    ok.
