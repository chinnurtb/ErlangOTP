-module(ttt).
-export([init/0]).

%% initialise and start a Tick Tac Toe Game

init() -> 
	% start graphics
	S = gs:start(),
	% initialise random number generator
	{A,B,C} = erlang:now(),
	random:seed(A,B,C),
	Win = gs:create(window, S, [{width, 300}, {height, 350}, {title, 'Tic Tac Toe'}]),
	% create control buttons
	gs:create(button, reset, Win, [{x,0},{y,300},{width,100},{height,50},{label, {text, "Reset"}}]),
	gs:create(button, quit, Win, [{x,100},{y,300},{width,100},{height,50},{label, {text, "Quit"}}]),
	gs:create(button, play, Win, [{x,200},{y,300},{width,100},{height,50},{label, {text, "Play"}}]),
	% create playing field
	Sq1 = makebutton(Win,0,0),
	Sq2 = makebutton(Win,0,1),
	Sq3 = makebutton(Win,0,2),
	Sq4 = makebutton(Win,1,0),
	Sq5 = makebutton(Win,1,1),
	Sq6 = makebutton(Win,1,2),
	Sq7 = makebutton(Win,2,0),
	Sq8 = makebutton(Win,2,1),
	Sq9 = makebutton(Win,2,2),
	Field = [Sq1, Sq2, Sq3, Sq4, Sq5, Sq6, Sq7, Sq8, Sq9],
	% show all
	gs:config(Win, {map, true}),
	reset(Field),
	% Process
	loop(Win, red, Field).

%% make a button 100 x 100 at location X, Y

makebutton(Win, X, Y) ->
	gs:create(button, Win, [{x,100*X},{y,100*Y},{width,100},{height,100}]).

%% set all the buttons on the playing field back to their default values

reset(L) ->
	playopt(L, [{bg, grey}, {enable, true}, {label, {text, ''}}]).

%% lock all the buttons on the playing field so no further actions are seen

lock(L) ->
	playopt(L, [{enable, false}]).

%% visit each button named in the list an set its options to X

playopt([H|T], X) ->
	gs:config(H, X),
	playopt(T, X);
playopt([], _) ->
	true.

%% count all the buttons in the list with grey backgrounds

cntgrey([H|T]) ->
	Color = gs:read(H, bg),
	if 
		Color == grey ->
			cntgrey(T) + 1;
		true ->
			cntgrey(T)
	end;
cntgrey([]) ->
	0.

%% visit each button in the list, read its background color and return a list

pollfield([H|T]) ->
	Color = gs:read(H, bg),
	[Color | pollfield(T)];
pollfield([]) ->
	[].

%% see if there are three buttons in a row which are of one color

threerow([X,X,X,_,_,_,_,_,_]) when X =/= grey -> 
	X;
threerow([_,_,_,X,X,X,_,_,_]) when X =/= grey -> 
	X;
threerow([_,_,_,_,_,_,X,X,X]) when X =/= grey -> 
	X;
threerow([X,_,_,X,_,_,X,_,_]) when X =/= grey -> 
	X;
threerow([_,X,_,_,X,_,_,X,_]) when X =/= grey -> 
	X;
threerow([_,_,X,_,_,X,_,_,X]) when X =/= grey -> 
	X;
threerow([X,_,_,_,X,_,_,_,X]) when X =/= grey -> 
	X;
threerow([_,_,X,_,X,_,X,_,_]) when X =/= grey -> 
	X;
threerow(X) ->
	false.

%% make a move for Color

play(Field, Color) ->
	N = random:uniform(9),
	Sq = lists:nth(N, Field),
	SqC = gs:read(Sq, bg),
	C = cntgrey(Field),
	if 
		SqC == grey ->
			setsquare(Sq, Color);
		C == 0 ->
			true;
		true ->
			play(Field, Color)
	end.

%% set a button's color and label and disable it

setsquare(B, C) ->
	gs:config(B, [{bg, C},{enable,false},{label, {text, C}}]).

%% check to see if any one has won and process events

loop(Win, Color, Field) ->
	Result = threerow(pollfield(Field)),
	if
		Result == red ->
			gs:config(play, [{enable, false}]),
			io:format("Red Wins~n", []),
			lock(Field);
		Result == blue ->
			gs:config(play, [{enable, false}]),
			io:format("Blue Wins~n", []),
			lock(Field);
		true ->
			true
	end,
	receive
		{gs, play, click, Data, Args} ->
			play(Field, Color),
			if 
				Color == red -> 
					loop(Win, blue, Field);
				true ->
					loop(Win, red, Field)
			end;
		{gs, reset, click, Data, Args} ->
			gs:config(play, [{enable, true}]),
			reset(Field),
			loop(Win, red, Field);
		{gs, quit, click, Data, Args} ->
			gs:destroy(win),
			gs:stop();
		{gs, Butt, click, Data, Args} ->
			setsquare(Butt, Color),
			if 
				Color == red -> 
					loop(Win, blue, Field);
				true ->
					loop(Win, red, Field)
			end;
		{gs, Butt, destroy, Data, Args} ->
			gs:stop()
	end.
