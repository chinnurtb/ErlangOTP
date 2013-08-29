-module(nofstale).
-export([start/0, nofailstale/0, vers/0]).

vers() ->
    3.

start() ->
    spawn(nofstale, nofailstale, []).

nofailstale() ->
    Msg = receive
	X -> X
    end,
    nofailstale(Msg).

nofailstale(new) ->
    code:purge(nofstale),
    code:load_file(nofstale),
    nofstale:nofailstale();
nofailstale(ver) ->
    io:format("Start Version ~w~n", [vers()]),
    nofailstale().
