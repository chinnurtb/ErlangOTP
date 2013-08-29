-module(chboard).
-export([start/0]).

start() ->
  Server = gs:start(),
  Win = gs:create(window, Server, [{width, 8 * sx()}, 
    {height, 100 + 8 * sy()}]),
  menu(Win),
  Canvas = newgame(Win),
  gs:config(Win, {map, true}),
  event_loop(Win, Canvas, normal, {}).

event_loop(Win, Canvas, Mode, StateData) ->
  receive
    {gs, exit, click, _, _} ->
      gs:stop(),
      exit(normal);
    {gs, new, click, _, _} ->
      gs:destroy(Canvas),
      event_loop(Win, newgame(Win), normal, {});
    {gs, Id, buttonpress, {X, Y, Base, Color, man, I}, 
      [ 3, _, _ | _ ]} when Mode == normal ->
      gs:destroy(I), 
      gs:config(Id, [{data, {X, Y, Base, empty, empty, noimage}}]),
      event_loop(Win, Canvas, Mode, StateData);
    {gs, Id, buttonpress, {X, Y, Base, Color, man, I}, 
      [ 2, _, _ | _ ]} when Mode == normal ->
      gs:destroy(I), 
      piece(Id, X, Y, Base, Color, king),
      event_loop(Win, Canvas, Mode, StateData);
    {gs, Id, buttonpress, {X, Y, Base, Color, Piece, I}, 
      [ 1, _, _ | _ ]} when Mode == normal, Piece =/= empty ->
      gs:destroy(I), 
      gs:config(Id, [{data, {X, Y, Base, empty, empty, noimage}}]),
      event_loop(Win, Canvas, move, {Id, X, Y, Base, Color, Piece});
    {gs, Id, enter, _, _ } when Mode == move ->
      gs:config(Id, {bg, red}),
      event_loop(Win, Canvas, move, StateData);
    {gs, Id, leave, {X, Y, Base, Color, Piece, I}, 
      _ } when Mode == move ->
      gs:config(Id, {bg, Base}),
      event_loop(Win, Canvas, move, StateData);
    {gs, Id, buttonpress, {X, Y, Base, Color, Piece, I}, 
      [ 1, _, _ | _ ]} when Mode == move, Piece == empty ->
      {OId, _, _, B, C, M} = StateData,
      gs:config(Id, [{bg, Base}]),
      piece(Id, X, Y, Base, C, M),
      event_loop(Win, Canvas, normal, {});
    X -> 
      event_loop(Win, Canvas, Mode, StateData)
  end.

menu(Win) ->
  MenuBar = gs:create(menubar, Win, []),
  FileBut = gs:create(menubutton, MenuBar, [{label, 
    {text, "File"}}]),
  FileMenu = gs:create(menu, FileBut, []),
  gs:create(menuitem, new, FileMenu, [{label, {text, "New"}}]),
  gs:create(menuitem, exit, FileMenu, [{label, {text, "Exit"}}]).

newgame(Win) ->
  Frame = gs:create(frame, Win, [{width, 8 * sx()}, 
    {height, 8 * sy()}, {bw, 1}, {x, 0}, {y, 100}]),
  drawch(Frame, 8, 8),
  setboard(Frame),
  Frame.

setboard(Frame) ->
  StartPos = [
  {1, 0, white, man}, {3, 0, white, man}, {5, 0, white, man}, 
  {7, 0, white, man}, {0, 1, white, man}, {2, 1, white, man}, 
  {4, 1, white, man}, {6, 1, white, man}, {1, 2, white, man}, 
  {3, 2, white, man}, {5, 2, white, man}, {7, 2, white, man},
  {0, 5, black, man}, {2, 5, black, man}, {4, 5, black, man}, 
  {6, 5, black, man}, {1, 6, black, man}, {3, 6, black, man}, 
  {5, 6, black, man}, {7, 6, black, man}, {0, 7, black, man}, 
  {2, 7, black, man}, {4, 7, black, man}, {6, 7, black, man}
  ],
  SqLst = gs:read(Frame, children),
  setboard(SqLst, StartPos).

setboard(SqLst, []) ->
  true;
setboard([Sq | T], Pieces) ->
  SqD = gs:read(Sq, data),
  setboard(T, setsq(Sq, SqD, Pieces, [])).

setsq(Sq, SqD, [], L) ->
  L;
setsq(Sq, {SqX, SqY, SqB, SqC, SqM, I}, [{X, Y, C, M} | T], L) ->
  if 
    SqX == X, SqY == Y ->
      piece(Sq, SqX, SqY, SqB, C, M),
      lists:append(T, L);
    true ->
      setsq(Sq, {SqX, SqY, SqB, SqC, SqM, I}, T, 
        [{X, Y, C, M} | L])
  end.

drawch(Frame, Nx, Ny) ->
  drawch(Frame, Nx, Ny, Nx-1, Ny-1, white).

drawch(F, Nx, Ny, 0, 0, BW) ->
  sq(F, 0, 0, BW);
drawch(F, Nx, Ny, Px, 0, BW) ->
  sq(F, Px, 0, BW),
  drawch(F, Nx, Ny, Px-1, Ny-1, BW);
drawch(F, Nx, Ny, Px, Py, BW) ->
  sq(F, Px, Py, BW),
  drawch(F, Nx, Ny, Px, Py-1, oppcolor(BW)).

sq(F, X, Y, Color) ->
  Xs = sx(),
  Ys = sy(),
  gs:create(canvas, F, [{x, X * Xs}, {y, Y * Ys}, 
    {width, Xs}, {height, Ys}, {bg, Color},
    {enter, true}, {leave, true}, {buttonpress, true},
    {data, {X, Y, Color, empty, empty, noimage}}]).

piece(Canvas, X, Y, Base, Color, Piece) ->
  File = case {Color, Piece} of
    {white, man} -> "whtbit.gif";
    {black, man} -> "blkbit.gif";
    {white, king} -> "whtking.gif";
    {black, king} -> "blkking.gif"
  end,
  I = gs:create(image, Canvas, [{load_gif, File}]),
  gs:config(Canvas, {data, {X, Y, Base, Color, Piece, I}}).

oppcolor(white) -> 
  black;
oppcolor(black) -> 
  white.

sx() -> 50.

sy() -> 50.
