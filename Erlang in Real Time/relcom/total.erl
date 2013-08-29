-module(total).
-export([start/0, read/1, add/2, tallier/2]).

timeout() -> 10000.
retry() -> 100000.

start() ->
	Pid = spawn(total, tallier, [0, 0]),
	sawarq:open(Pid, timeout(), retry()).

read(Connect) ->
	Connectp = sawarq:xmit(Connect, {read, self()}),
	Result = receive 
		X -> X
	after timeout() ->
		error
	end,
	{Connectp, Result}.

add(Connect, T) ->
	sawarq:xmit(Connect, {add, T}).

tallier(N, Total) ->
	{Np, Msg} = sawarq:recv(N),
	case Msg of
		{read, Pid} ->
			Pid  ! Total,
			tallier(Np, Total);
		{add, T} ->
			tallier(Np, Total + T);
		_ ->
			tallier(Np, Total)
	end.
