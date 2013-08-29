-module(wi).
-import(gs, [create/3, config/2]).
-export([init/0]).

init() -> 
    S = gs:start(),
    Win = create(window, S, [{width, 200}, {height, 200}, 
    {title, 'Attack of the Wobbly Invaders'}]),
    Canvas = create(canvas, Win, [{width, 200}, {height, 200}, 
    {bg, white}]),
    config(Win, {map, true}),
    loop(Canvas, 0, 0, 1).


loop(C, X, Y, P) ->
    drawwi(C, X, Y, P),
    receive 
        {gs, Id, destroy, Data, Arg} ->
            bye
    after 500 ->
        erasewi(C, X, Y),
        if
            Y == 200 ->
                bye;
            X == 200 -> 
                loop(C, 0, Y+20, -P);
            true ->
                loop(C, X+20, Y, -P)
        end
    end.

drawwi(C, X, Y, 1) ->
    create(image,C,[{load_gif,"thing1.gif"}, {coords, [{X,Y}]}]);
drawwi(C, X, Y, -1) ->
    create(image,C,[{load_gif,"thing2.gif"}, {coords, [{X,Y}]}]).

erasewi(C, X, Y) ->
    create(rectangle,C,[{coords,[{X,Y},{X+20,Y+20}]}, 
    {fg, white}, {fill,white}]).
