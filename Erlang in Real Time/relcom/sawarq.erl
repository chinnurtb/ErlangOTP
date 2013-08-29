-module(sawarq).
-export([open/3, xmit/2, recv/1, procexists/1]).

open(Dest, Timeout, Retry) ->
    RefId = make_ref(),
    {Dest, RefId, 0, Timeout, Retry}.

xmit({Dest, RefId, N, Timeout, Retry}, Mesg) ->
    Dest ! {self(), RefId, N, Mesg},
    receive
        {RefId, N, ack} ->
            {Dest, RefId, (N+1) rem 2, Timeout, Retry};
        {RefId, OtherN, ack} ->
            error
    after Timeout ->
        case procexists(Dest) of
            true -> 
                receive
                after Retry ->
                    ok
                end,
                xmit({Dest, RefId, N, Timeout, Retry}, Mesg);
            _ ->
                error
        end
    end.

recv(N) ->
    receive
        {Sender, RefId, N, Mesg} ->
            Sender ! {RefId, N, ack},
            {(N+1) rem 2, Mesg};
        {Sender, RefId, _, Mesg} ->
            error
    end.

procexists(Pid) when pid(Pid) ->
    Nd = node(Pid),
    ThisNd = node(),
    ListProc = if
        Nd == ThisNd ->
            processes();
        true ->
            rpc:call(Nd, erlang, processes, [])
    end,
    lists:member(Pid, ListProc);
procexists(Name) ->
    case whereis(Name) of
        undefined -> false;
        _ -> true
    end.
