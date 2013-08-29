=================================================
Hacking Through The Erlang Wilderness : Episode 5
=================================================

.. footer:: Copyright (c) 2011 Todd D. Greenwood-Geer 

:Author: Todd D. Greenwood-Geer, Sean Jensen-Grey
:Date: Tue March 15  2011
:Version: 0.2
:Index: Index_ : Listing of all the episodes


----------------------------------------
How to Create a Webmachine REST Endpoint
----------------------------------------

Goal
----

Create a REST endpoint that will add two integers and return the result.

Example::

    http://erlang32:8000/service/add/1/2

In this case, this should return 3 as a text/plain response.


Create new Rest Endpoint
------------------------

1. We're starting where we left off in the previous episode, Episode-04_.

2. Take a look at the generated and built files::

    ├── ebin
    │   ├── mywebdemo.app
    │   ├── mywebdemo_app.beam
    │   ├── mywebdemo.beam
    │   ├── mywebdemo_resource.beam
    │   └── mywebdemo_sup.beam
    ├── Makefile
    ├── priv
    │   ├── dispatch.conf
    │   ├── log
    │   │   ├── access.log.2011_03_16_03
    │   │   └── access.log.2011_03_16_04
    │   └── www
    ├── README
    ├── rebar
    ├── rebar.config
    ├── src
    │   ├── mywebdemo_app.erl
    │   ├── mywebdemo.app.src
    │   ├── mywebdemo.erl
    │   ├── mywebdemo_resource.erl
    │   └── mywebdemo_sup.erl
    └── start.sh

3. Examine the dispatch.conf::

    user@erlang32:~/projects/webmachine/mywebdemo$ cat priv/dispatch.conf 
    %%-*- mode: erlang -*-
    {[], mywebdemo_resource, []}.

4. Check out the docs, at: http://webmachine.basho.com/dispatcher.html

5. Add a simple service endpoint, called 'bilbo'::

    user@erlang32:~/projects/webmachine/mywebdemo$ cat priv/dispatch.conf 
    %%-*- mode: erlang -*-
    {[], mywebdemo_resource, []}.
    {["bilbo"], bilbo_resource, []}.


6. Add the resource implementation to src/bilbo_resource.erl::

    %% @author author <author@example.com>
    %% @copyright YYYY author.
    %% @doc Example webmachine_resource.

    -module(bilbo_resource).
    -export([init/1, to_html/2]).

    -include_lib("webmachine/include/webmachine.hrl").

    init([]) -> {ok, undefined}.

    to_html(ReqData, State) ->
        {"<html><body>Bilbo!!!</body></html>", ReqData, State}.

7. Restart erlang::

    user@erlang32:~/projects/webmachine/mywebdemo$ ./rebar compile
    ==> mochiweb (compile)
    ==> webmachine (compile)
    ==> mywebdemo (compile)
    Compiled src/service_resource.erl
    user@erlang32:~/projects/webmachine/mywebdemo$ ./start.sh 

8. Open in a browser from host::

    [host] $ open http://erlang32:8000/bilbo

.. image:: https://github.com/ToddG/experimental/raw/master/erlang/wilderness/05/images/bilbo.png

Add Text Endpoint
-------------------

1. Create a new resource and set the result to be text/plain, src/text_resource.erl::

    %% @author author <author@example.com>
    %% @copyright YYYY author.
    %% @doc Example webmachine_resource.

    -module(text_resource).
    -export([init/1, to_text/2, content_types_provided/2]).
    -import(string).

    -include_lib("webmachine/include/webmachine.hrl").

    init([]) -> {ok, undefined}.

    content_types_provided(ReqData, Context) ->
        {[{"text/plain",to_text}], ReqData, Context}.


    %%to_html(ReqData, State) ->
    %%    {"<html><body>Hello from service </body></html>", ReqData, State}.

    to_text(ReqData, State) ->
        {"<html><body>Text Resource</body></html>", ReqData, State}.

2. Add a resource mapping in priv/dispatch.erl::

    {["text"], text_resource, []}.

3. Rebuild

::

    user@erlang32:~/projects/webmachine/mywebdemo$ ./rebar compile
    ==> mochiweb (compile)
    ==> webmachine (compile)
    ==> mywebdemo (compile)

4. Start erlang

::

    user@erlang32:~/projects/webmachine/mywebdemo$ ./start.sh 

5. Open in browser::

    [host] $ open http://erlang32:8000/text

.. image:: https://github.com/ToddG/experimental/raw/master/erlang/wilderness/05/images/text.png


Add a Calculator Service as an Endpoint
----------------------------------------


1. Update the priv/dispatch.conf to take map part of the query string to an atom like so::

    {["calculator",key,'*'], calculator_resource, []}.

* this will match this URI::

    http://[some server]/calculator/ABC/whatever

============    ==============
"service"       service
key             ABC
'*'             whatever
============    ==============


2. Current priv/dispatch.conf::

    %%-*- mode: erlang -*-
    {[], mywebdemo_resource, []}.
    {["bilbo"], bilbo_resource, []}.
    {["text"], text_resource, []}.
    {["calculator", key, '*'], calculator_resource, []}.


13. Implement an 'add' method for our generic 'service' endpoint in the to_text method in src/service_resource.erl

::

    %% @author author <author@example.com>
    %% @copyright YYYY author.
    %% @doc Example webmachine_resource.
    -module(calculator_resource).
    -export([init/1, to_text/2, content_types_provided/2]).
    -import(string).
    -include_lib("webmachine/include/webmachine.hrl").
    init([]) -> {ok, undefined}.
    content_types_provided(ReqData, Context) ->
        {[{"text/plain",to_text}], ReqData, Context}.
    add_tokens(Tokens) ->
        {A,_} = string:to_integer(lists:nth(1,Tokens)),
        {B,_} = string:to_integer(lists:nth(2,Tokens)),
        string:join([integer_to_list(A), "+", integer_to_list(B), "=", integer_to_list(A+B)], " ").
    to_text(ReqData, State) ->
        Key = wrq:path_info(key,ReqData),
        case Key of
            undefined ->
                {"usage: [add] : calculator/add/number1/number2 ", ReqData, State};
            Value ->
                case Value of
                    "add" ->
                        Tokens = wrq:path_tokens(ReqData),
                        io:format("calculator command :'add', path_tokens:~p~n", [Tokens]),
                        { add_tokens(Tokens), ReqData, State };
                    _ ->
                        {"calculator/add/firstNumber/secondNumber : returns firstNumber + secondNumber", ReqData, State }
                end
        end.

14. Compile, start the service, and point your browser at http://erlang32:8000/calculator/add

.. image:: https://github.com/ToddG/experimental/raw/master/erlang/wilderness/05/images/server_error.png

Ok, in the future, I'll add better error handling.

15. Try adding with this: http://erlang32:8000/calculator/add/1/2

.. image:: https://github.com/ToddG/experimental/raw/master/erlang/wilderness/05/images/one_plus_2.png

16. And more adding

.. image:: https://github.com/ToddG/experimental/raw/master/erlang/wilderness/05/images/more_adding.png

17. Finally, look at the io:format statements in the server log

.. image:: https://github.com/ToddG/experimental/raw/master/erlang/wilderness/05/images/server_log.png

18. Done.


References
==========

.. [ARMSTRONG]
    Armstrong, Joe.
    Programming Erlang
    The Pragmatic Bookshelf, 2007. ISBN 978-1-934356-00-5

.. [CESARINI] 
    Cesarini, Francesco, Thompson, Simon.
    Erlang Programming
    O'Reily, 2009. ISBN 978-0-596-51818-9

.. [LOGAN]
    Logan, Martin, Merritt, Eric, Carlsson, Richard.
    Erlang and OTP in Action
    Manning, 2011. ISBN 9781933988788

.. _ErlDocs_Logger: http://erldocs.com/R14B01/kernel/error_logger.html?i=91

.. _SinanProjects: http://erlware.github.com/sinan/SinanProjects.html

.. _Sinan_Faxien_Demo: http://www.youtube.com/watch?v=XI7S2NwFPOE

.. _Basho_Rebar_Demo: http://blog.basho.com/category/rebar/

.. _Erlware: http://erlware.com/

.. _Rebar: https://bitbucket.org/basho/rebar/wiki/GettingStarted

.. _Index: https://github.com/ToddG/experimental/tree/master/erlang/wilderness

.. _Episode-00: https://github.com/ToddG/experimental/tree/master/erlang/wilderness/00/

.. _Episode-02: https://github.com/ToddG/experimental/tree/master/erlang/wilderness/02
.. _Episode-04: https://github.com/ToddG/experimental/tree/master/erlang/wilderness/04

.. _Calendar: http://erldocs.com/R14B01/stdlib/calendar.html?i=230

.. _Eunit: http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html
