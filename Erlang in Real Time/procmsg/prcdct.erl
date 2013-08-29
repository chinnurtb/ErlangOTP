-module(prcdct).
-export([teldir/0]).

teldir() -> getdata(), menu(), querylp().

getdata() ->
    io:format("Enter name and phone number ", []),
    io:format("(blank line to end)~n", []),
    getlp().

getlp() ->
    Line = io:get_line('name phone> '),
    Lst = string:tokens(Line, " \n"),
    getlp(Lst).

getlp([Name, Phone]) ->
    put(Name, Phone),
    getlp();

getlp(Lst) when length(Lst) == 0 ->
    true;
getlp(_) ->
    io:format("Error~n"),
    getlp().

menu() -> 
    io:format("Operation~n 1) Search 2) Add/Change "),
    io:format("3) List Names 4) Delete    0) Quit~n", []).

querylp() -> querylp(io:fread('op > ', "~d")).

querylp({ok, [0]}) -> true;
querylp({ok, [1]}) -> search(), querylp();
querylp({ok, [2]}) -> getdata(), querylp();
querylp({ok, [3]}) -> lstname(), querylp();
querylp({ok, [4]}) -> delete(), querylp().

getnam() ->
    Line = io:get_line('name > '),
    getnam(string:tokens(Line, " \n")).

getnam([L]) -> L;
getnam(_) -> io:format("Error~n"), getnam().

search() -> io:format("~s~n", [get(getnam())]).

lstname() -> lstname(get()).

lstname([]) -> true;
lstname([{Key, Value}|T]) -> io:format("~s~n", [Key]), lstname(T).

delete() -> io:format("~s~n", [erase(getnam())]).
