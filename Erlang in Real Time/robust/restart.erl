-module(restart).
-export([start/0, init/0, add/3, remove/3]).

start() ->
    spawn(restart, init, []).

init() ->
    process_flag(trap_exit, true),
    register(restart, self()),
    manage([]).

add(M, F, A) ->
    restart ! {add, M, F, A}.

remove(M, F, A) ->
    restart ! {remove, M, F, A}.

manage(ListProc) ->
    NewList = receive
        {add, Moda, Funa, Arga} ->
            case (catch newproc(Moda, Funa, Arga, ListProc)) of
                X when list(X) ->
                    X;
                _ ->
                    ListProc
            end;
        {remove, Modr, Funr, Argr} ->
            remproc(Modr, Funr, Argr, ListProc);
        {'EXIT', Pid, Reason} ->
            restart(Pid, ListProc);
        Unknown ->
            ListProc
    end,
    manage(NewList).

newproc(M, F, A, L) -> 
    Pid = spawn_link(M, F, A),
    [{M, F, A, Pid} | L].

remproc(M, F, A, L) ->
    remproc(M, F, A, L, []).

remproc(M, F, A, [], L) ->
    L;
remproc(M, F, A, [{HM, HF, HA, HP}|T], L) 
        when M==HM, F==HF, A==HA ->
    remproc(M, F, A, T, L);
remproc(M, F, A, [H|T], L) ->
    remproc(M, F, A, T, [H|L]).

restart(P, L) ->
    restart(P, L, []).

restart(P, [], L) ->
    L;
restart(P, [{HM, HF, HA, HP}|T], L) when P==HP ->
    NL = case (catch newproc(HM, HF, HA, T)) of
        X when list(X) ->
            X;
        _ ->
            T
    end,
    lists:append(NL, L);
restart(P, [H|T], L) ->
    restart(P, T, [H|L]).
