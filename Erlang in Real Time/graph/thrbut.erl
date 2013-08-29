-module(thrbut).
-export([init/0]).

init() ->
    Server = gs:start(),
    Win = gs:create(window, Server, [{width, 300}, {height, 80}]),
    Display = gs:create(label, Win, [{label, {text, "0"}}, 
        {x, 0}, {y, 0}, {width, 200}, {height, 50}]),
    Plus = gs:create(button, Win, [{label, {text, "+"}}, 
        {x, 0}, {y, 50}]),
    Minus = gs:create(button, Win, [{label, {text, "-"}}, 
        {x, 100}, {y, 50}]),
    Quit = gs:create(button, Win, [{label, {image, "q.xbm"}}, 
        {x, 200}, {y, 50}]),
    gs:config(Win, {map, true}),
    event_loop(0, Display, Plus, Minus, Quit).

event_loop(N, D, P, M, Q) ->
    receive
        {gs, P, click, Data, Args} ->
            RP = N+1,
            gs:config(D, {label, {text, RP}}),
            event_loop(RP, D, P, M, Q);
        {gs, M, click, Data, Args} ->
            RM = N-1,
            gs:config(D, {label, {text, RM}}),
            event_loop(RM, D, P, M, Q);
        {gs, Q, click, Data, Args} ->
            gs:stop(),
            N
    end.
