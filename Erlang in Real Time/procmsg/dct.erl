-module(dct).
-export([start/0,get/2,put/3,dct/1]).

start() ->
    spawn(dct,dct,[[]]).

get(Pid, Key) ->
    Pid ! {get, self(), Key},
    receive
        X -> X
    end.

put(Pid, Key, Value) ->
    Pid ! {put, self(), Key, Value},
    receive
        X -> X
    end.

dct(L) ->
    NewL = receive
        {put, Pid, Key, Value} ->
            {Code, List} = insert(L, [], Key, Value),
            Pid ! Code,
            List;
        {get, Pid, Key} ->
            Code = find(L, [], Key),
            Pid ! Code,
            L;
        X -> 
            L
    end,
    dct(NewL).

insert([], N, Key, Value) ->
    {undefined, [{Key, Value}|N]};
insert([{Hkey, HVal} |T], N, Key, Value) when Key == Hkey ->
    {HVal, lists:append(N,[{Key, Value} | T])};
insert([H|T], N, Key, Value) ->
    insert(T, [H|N], Key, Value).

find([], N, Key) ->
    undefined;
find([{Hkey, HVal}|T], N, Key) when Key == Hkey ->
    HVal;
find([H|T], N, Key) ->
    find(T, [H|N], Key).
