-module(ex02).
-export([ hello/0, hello/1, hello/2 ]).

hello() ->
    io:format("Hello World!~n").

hello(Name) ->
    io:format("Hello ~s!~n", [Name] ).

hello(Greeting, Name) ->
    io:format("~s, ~s!~n", [Greeting, Name] ).

