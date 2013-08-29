=================================================
Hacking Through The Erlang Wilderness : Episode 3 
=================================================

.. footer:: Copyright (c) 2011 Todd D. Greenwood-Geer 

:Author: Todd D. Greenwood-Geer
:Date: Tue Feb 13  2011
:Version: 0.1
:Index: Index_ : Listing of all the episodes


----------------------------
How to Create a Time Server
----------------------------

Review
------

So in the previous installment, we created an application named 'app01'. To this empty application, we added a gen_server behaviour, 'src/app01.erl'. We had to modify the 'src/app01_sup.erl' to reference the behaviour. Lastly, we had to update the 'ebin/app01.app' to include this behaviour in the top level application definition:

 * src/app01_sup.erl : code that determines the lifecycle of our application, restarts it if it dies and so on
 * src/app01_app.erl : code that describes our application, has a start() and stop() methods
 * src/app01.erl : a gen_server behavior that defines what our app *is* and *does*
 * ebin/app01.app : the top level definition of our application, modules are specified here

To go more in-depth on project structure and whatnot, check these out:

 * SinanProjects_ : shows single and multi app projects
 * [LOGAN]_ : page 119 starts the chapter on 'OTP applications and supervision' and lays out application structure


Time Server Basics
------------------

We're going to create an app that spits back the time. Super simple. Along the way, we'll 

 * instrument the app with print statements so we can see what's going on inside
 * add some eunit tests
 * play more with sinan and rebar

1. Create new application, 'time_srv'

::

    todd@ubuntu:~/temp$ sinan gen time_srv

::

    starting: gen
    Please specify your name 
    your name> Todd Greenwood-Geer
    Please specify your email address 
    your email> todd@niovb.com
    Please specify the copyright holder 
    copyright holder ("Todd Greenwood-Geer")> 
    Please specify version of your project
    project version> 0.0.1
    Please specify the ERTS version ("5.8.2")> 
    Is this a single application project ("n")> y
    /home/todd/temp/time_srv/doc created ok.
    /home/todd/temp/time_srv/bin created ok.
    /home/todd/temp/time_srv/config created ok.
    /home/todd/temp/time_srv/ebin created ok.
    /home/todd/temp/time_srv/src created ok.
    /home/todd/temp/time_srv/include created ok.
    /home/todd/temp/time_srv/doc exists ok.
    Would you like a build config? ("y")> 
    Project was created, you should be good to go!
    
    
2. Add the gen_server behaviour to this application

::
    
    todd@ubuntu:~/temp$ cd time_srv/
    todd@ubuntu:~/temp/time_srv$ wget http://bitbucket.org/basho/rebar/downloads/rebar; chmod u+x rebar

::

    --2011-02-13 10:49:30--  http://bitbucket.org/basho/rebar/downloads/rebar
    Resolving bitbucket.org... 207.223.240.181, 207.223.240.182
    Connecting to bitbucket.org|207.223.240.181|:80... connected.
    HTTP request sent, awaiting response... 301 Moved Permanently
    Location: https://bitbucket.org/basho/rebar/downloads/rebar [following]
    --2011-02-13 10:49:30--  https://bitbucket.org/basho/rebar/downloads/rebar
    Connecting to bitbucket.org|207.223.240.181|:443... connected.
    HTTP request sent, awaiting response... 302 FOUND
    Location: http://cdn.bitbucket.org/basho/rebar/downloads/rebar [following]
    --2011-02-13 10:49:30--  http://cdn.bitbucket.org/basho/rebar/downloads/rebar
    Resolving cdn.bitbucket.org... 216.137.35.70, 216.137.35.73, 216.137.35.249, ...
    Connecting to cdn.bitbucket.org|216.137.35.70|:80... connected.
    HTTP request sent, awaiting response... 200 OK
    Length: 85084 (83K) [binary/octet-stream]
    Saving to: `rebar'

    100%[===============================================================================>] 85,084      --.-K/s   in 0.1s    
    2011-02-13 10:49:30 (839 KB/s) - `rebar' saved [85084/85084]

If you download the source to rebar, you can check out the template variables. We'll use one now...

What are the rebar commands?

::

    todd@ubuntu:~/temp/time_srv$ rebar -c

    analyze                              Analyze with Dialyzer
    build_plt                            Build Dialyzer PLT
    check_plt                            Check Dialyzer PLT

    clean                                Clean
    compile                              Compile sources

    create      template= [var=foo,...]  Create skel based on template and vars
    create-app  [appid=myapp]            Create simple app skel
    create-node [nodeid=mynode]          Create simple node skel
    list-templates                       List available templates

    doc                                  Generate Erlang program documentation

    check-deps                           Display to be fetched dependencies
    get-deps                             Fetch dependencies
    delete-deps                          Delete fetched dependencies

    generate    [dump_spec=0/1]          Build release with reltool

    eunit       [suite=foo]              Run eunit [test/foo_tests.erl] tests
    ct          [suite=] [case=]         Run common_test suites in ./test

    xref                                 Run cross reference analysis

    help                                 Show the program options
    version                              Show version information

What templates are available again?

::

    todd@ubuntu:~/temp/time_srv$ rebar list-templates
    ==> time_srv (list-templates)
    Available templates:
            * simplesrv: priv/templates/simplesrv.template (escript)
            * simplenode: priv/templates/simplenode.template (escript)
            * simplemod: priv/templates/simplemod.template (escript)
            * simplefsm: priv/templates/simplefsm.template (escript)
            * simpleapp: priv/templates/simpleapp.template (escript)
            * basicnif: priv/templates/basicnif.template (escript)

Cool, let's make create a file from a template

::

    todd@ubuntu:~/temp/time_srv$ rebar create template=simplesrv srvid=time_srv
    ==> time_srv (create)
    Writing src/time_srv.erl

And the file we just created, 'src/time_srv.erl'

::

    todd@ubuntu:~/temp/time_srv$ cat src/time_srv.erl 
    -module(time_srv).
    -behaviour(gen_server).
    -define(SERVER, ?MODULE).

    %% ------------------------------------------------------------------
    %% API Function Exports
    %% ------------------------------------------------------------------

    -export([start_link/0]).

    %% ------------------------------------------------------------------
    %% gen_server Function Exports
    %% ------------------------------------------------------------------

    -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

    %% ------------------------------------------------------------------
    %% API Function Definitions
    %% ------------------------------------------------------------------

    start_link() ->
      gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    %% ------------------------------------------------------------------
    %% gen_server Function Definitions
    %% ------------------------------------------------------------------

    init(Args) ->
      {ok, Args}.

    handle_call(_Request, _From, State) ->
      {noreply, ok, State}.

    handle_cast(_Msg, State) ->
      {noreply, State}.

    handle_info(_Info, State) ->
      {noreply, State}.

    terminate(_Reason, _State) ->
      ok.

    code_change(_OldVsn, State, _Extra) ->
      {ok, State}.

    %% ------------------------------------------------------------------
    %% Internal Function Definitions
    %% ------------------------------------------------------------------

3. Update the superviser, 'src/time_srv_sup.erl', by inserting references to time_srv in the child definition

::

     46     %AChild = {'AName', {'AModule', start_link, []},
     47     %          Restart, Shutdown, Type, ['AModule']},
     48     
     49     AChild = {time_srv, {time_srv, start_link, []},
     50               Restart, Shutdown, Type, [time_srv]},

4. Update the modules list in the application definition, 'ebin/time_srv.app'

::

      1 %% This is the application resource file (.app file) for the time_srv,
      2 %% application.
      3 {application, time_srv,
      4   [{description, "Your Desc HERE"},
      5    {vsn, "0.0.1"},
      6    {modules, [  time_srv,
      7                 time_srv_app,
      8                 time_srv_sup]},
      9    {registered,[time_srv_sup]},
     10    {applications, [kernel, stdlib]},
     11    {mod, {time_srv_app,[]}},
     12    {start_phases, []}]}.

I inserted the reference to the time_srv behaviour in line 6 above.

5. Run the app

 * Build::

    todd@ubuntu:~/temp/time_srv$ sinan build
    starting: depends
    starting: build
    Building /home/todd/temp/time_srv/src/time_srv_sup.erl
    Building /home/todd/temp/time_srv/src/time_srv_app.erl
    Building /home/todd/temp/time_srv/src/time_srv.erl

 * Run::

    todd@ubuntu:~/temp/time_srv$ sinan shell
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    starting: shell
    Eshell V5.8.2  (abort with ^G)
    1> application:start(time_srv).
    ok

 * Check process registry::

    2> regs().

    ** Registered procs on node nonode@nohost **
    Name                  Pid          Initial Call                      Reds Msgs
    time_srv              <0.100.0>    time_srv:init/1                     26    0
    time_srv_sup          <0.99.0>     supervisor:time_srv_sup/1          110    0
    timer_server          <0.67.0>     timer:init/1                       106    0

6. Ok, at this point, we're where we left off in Episode-02_. A copy of the code at this point is here under time-srv-01.

Add Logging
-----------

Let's instrument our app so that we can get a better idea of what's going on.

1. Add logging to src/time_srv.erl. 

*Note that I'm modifying the parameters by removing any "_" (underscore) prefixes to the variables. This means that instead of not being bound, these variables are bound and we can then print them out in our logging messages.*

::

     21 start_link() ->
     22   error_logger:info_msg("[~p] start_link()~n", [?MODULE]),
     23   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
     24 
     25 %% ------------------------------------------------------------------
     26 %% gen_server Function Definitions
     27 %% ------------------------------------------------------------------
     28 
     29 init(Args) ->
     30   error_logger:info_msg("[~p] init(~p)~n", [?MODULE, Args]),
     31   {ok, Args}.
     32 
     33 handle_call(Request, From, State) ->
     34   error_logger:info_msg("[~p] handle_call(~p,~p,~p)~n", [?MODULE, Request, From, State]),
     35   {noreply, ok, State}.
     36 
     37 handle_cast(Msg, State) ->
     38   error_logger:info_msg("[~p] handle_cast(~p,~p)~n", [?MODULE, Msg, State]),
     39   {noreply, State}.
     40 
     41 handle_info(Info, State) ->
     42   error_logger:info_msg("[~p] handle_info(~p,~p)~n", [?MODULE, Info, State]),
     43   {noreply, State}.
     44 
     45 terminate(Reason, State) ->
     46   error_logger:info_msg("[~p] terminate(~p,~p)~n", [?MODULE, Reason, State]),
     47   ok.
     48 
     49 code_change(OldVsn, State, Extra) ->
     50   error_logger:info_msg("[~p] code_change(~p,~p,~p)~n", [?MODULE, OldVsn, State,Extra]),
     51   {ok, State}.
     52 

2. Build the app and launch the erlang shell (but don't start the app yet)

::

    $ sinan build
    $ sinan shell

    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    starting: shell
    Eshell V5.8.2  (abort with ^G)


2. We're using the error_logger (see ErlDocs_Logger_)  instead of the io:format. This means we have to turn on logging in our node or we won't see anything.

::

    1> error_logger:tty(true).
    ok

3. Now start the application

::

    2> application:start(time_srv).

4. And bask in the glory of your new debug messages

::

    =INFO REPORT==== 18-Feb-2011::19:58:39 ===
    [time_srv] start_link()
    //  
    =INFO REPORT==== 18-Feb-2011::19:58:39 ===
    [time_srv] init([])
    3> ok 

5. Terminate the app, and look at the logging there

::

    4> application:stop(time_srv). 

::

    =INFO REPORT==== 18-Feb-2011::20:07:14 ===
        application: time_srv
        exited: stopped
        type: temporary
    1> ok

6. But this doesn't give us the full picture, let's instrument the app and the supervisor files, too.

First, instrument src/time_srv_app.erl

::

     23 start(StartType, StartArgs) ->                             
     24   error_logger:info_msg("[~p] start(~p,~p)~n", [?MODULE, StartType, StartArgs]),        
     25   case time_srv_sup:start_link() of                                                
     26       {ok, Pid} ->                                                                 
     27           {ok, Pid};                                                               
     28       Error ->        
     29           Error   
     30   end.                                                      
     31              
     32 %% @private                    
     33 -spec stop(State::any()) -> ok.      
     34 stop(State) ->                                                                               
     35   error_logger:info_msg("[~p] stop(~p)~n", [?MODULE, State]),                   
     36   ok.  

Second, instrument the supervisor in src/time_srv_sup.erl

::

     24 start_link() ->
     25   error_logger:info_msg("[~p] start_link()~n", [?MODULE]),
     26   supervisor:start_link({local, ?SERVER}, ?MODULE, []).
     27 
     28 %%%===================================================================
     29 %%% Supervisor callbacks
     30 %%%===================================================================
     31 
     32 
     33 %% @private
     34 -spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
     35                        ignore | {error, Reason::any()}.
     36 init([]) ->
     37   error_logger:info_msg("[~p] init([])~n", [?MODULE]),

7. Rebuild and relaunch the shell

::

    todd@ubuntu:~/temp/time_srv$ sinan build
    starting: depends
    starting: build
    Building /home/todd/temp/time_srv/src/time_srv_sup.erl
    Building /home/todd/temp/time_srv/src/time_srv_app.erl
    todd@ubuntu:~/temp/time_srv$ sinan shell
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    starting: shell
    Eshell V5.8.2  (abort with ^G)
    1> 

8. Turn on logging

::

    1> error_logger:tty(true).
    ok

9. Start the time_srv application

::

    2> application:start(time_srv).

    =INFO REPORT==== 18-Feb-2011::20:16:53 ===
    [time_srv_app] start(normal,[])
       
    =INFO REPORT==== 18-Feb-2011::20:16:53 ===
    [time_srv_sup] start_link()
       
    =INFO REPORT==== 18-Feb-2011::20:16:53 ===
    [time_srv_sup] init([])
       
    =INFO REPORT==== 18-Feb-2011::20:16:53 ===
    [time_srv] start_link()
       
    =INFO REPORT==== 18-Feb-2011::20:16:53 ===
    [time_srv] init([])
    1> ok    

So, you can trace the starting of an application:

    *app -> sup -> module*

10. Now stop the application

::

    =INFO REPORT==== 18-Feb-2011::20:18:45 ===
    [time_srv_app] stop([])
       
    =INFO REPORT==== 18-Feb-2011::20:18:45 ===
        application: time_srv
        exited: stopped
        type: temporary
    1> ok

What's interesting here is what does not get called. Apparently stopping an app invokes the time_srv_app.stop() method, but not the time_srv.terminate() method. Good to know. We'll use this logging as we start to implement more interesting functionality in our time server. The code, at this point is available under time-srv-02.
    
Add EUnit Tests
---------------


1. Let's add some simple tests, but first we need to add an import statement to src/time_srv.erl.

::

    1 -module(time_srv).
    2 -behaviour(gen_server).
    3 -define(SERVER, ?MODULE).
    4 
    5 -define(TEST, test).
    6 -ifdef(TEST).
    7 -include_lib("eunit/include/eunit.hrl").
    8 -endif. 

I explicitly define TEST and then ifdef based on it here and below in the test definitions. Note that we don't have any internal functions defined yet, and we've already got some tests. This test simply checks that our app can be started.

::

     59 %% ------------------------------------------------------------------
     60 %% Internal Function Definitions
     61 %% ------------------------------------------------------------------
     62 
     63 %% ------------------------------------------------------------------
     64 %% EUnit Tests
     65 %% ------------------------------------------------------------------
     66 -ifdef(TEST).
     67 
     68 simple_test() ->
     69     ok = application:start(time_srv),
     70     ?assertNot(undefined == whereis(time_srv_sup)).
     71 -endif.

2. Let's build and run the test

::

    todd@ubuntu:~/temp/time_srv$ sinan test
    starting: depends
    starting: build
    starting: test
    Testing time_srv
    time_srv:  Test passed.
    time_srv_app:  There were no tests to run.
    time_srv_sup:  There were no tests to run.

3. What happens when a test fails? Let's inject a failing test to see...

::

    72 failing_test() ->
    73     ok = false.

This results in a test failure...

::

    todd@ubuntu:~/temp/time_srv$ sinan test
    starting: depends
    starting: build
    Building /home/todd/temp/time_srv/src/time_srv.erl
    /home/todd/temp/time_srv/src/time_srv.erl:73:warning:this clause cannot match because of different types/sizes
    /home/todd/temp/time_srv/src/time_srv.erl:73:warning:no clause will ever match

    starting: test
    Testing time_srv
    time_srv:time_srv: failing_test...*failed*
    ::error:{badmatch,false}
      in function time_srv:failing_test/0


    =======================================================
      Failed: 1.  Skipped: 0.  Passed: 1.
    time_srv_app:  There were no tests to run.
    time_srv_sup:  There were no tests to run.

4. Remove the failing test, or comment it out. We know what that does now.

5. At long last, it's time for some Test Driven Development. Let's implement a function that actually does something...returns the current time. And the first thing we'll do is write a test for it. 

So, add the following test:

::

    77 get_current_time() ->
    78   {current_time, _ActualTime} = time_srv:get_time().

Now run the test...

::

    todd@ubuntu:~/temp/time_srv$ sinan test
    starting: depends
    starting: build
    Building /home/todd/temp/time_srv/src/time_srv.erl
    /home/todd/temp/time_srv/src/time_srv.erl:77:warning:function get_current_time/0 is unused

    starting: test
    Testing time_srv
    time_srv:  Test passed.
    time_srv_app:  There were no tests to run.
    time_srv_sup:  There were no tests to run.


6. Notice my goof? I forgot to suffix the test method with 'test'. So it looked like my test was an un-exported module method::

    /home/todd/temp/time_srv/src/time_srv.erl:77:warning:function get_current_time/0 is unused

7. Let's fix the name of this test::

    77 get_current_time_test() ->
    78   {current_time, _ActualTime} = time_srv:get_time().

8. Now run it

::

    todd@ubuntu:~/temp/time_srv$ sinan test
    starting: depends
    starting: build
    Building /home/todd/temp/time_srv/src/time_srv.erl
    starting: test
    Testing time_srv
    time_srv:time_srv: get_current_time_test...*failed*
    ::error:undef
      in function time_srv:get_time/0
        called as get_time()
      in call from time_srv:get_current_time_test/0


    =======================================================
      Failed: 1.  Skipped: 0.  Passed: 1.
    time_srv_app:  There were no tests to run.
    time_srv_sup:  There were no tests to run.

9. Excellent, our test caught the undeclared method. Let's declare it::

    17 -export([get_time/0]).

10. Now re-run

::

    todd@ubuntu:~/temp/time_srv$ sinan test
    starting: depends
    starting: build
    Building /home/todd/temp/time_srv/src/time_srv.erl
    /home/todd/temp/time_srv/src/time_srv.erl:17:error:function get_time/0 undefined
    
    build problem build_errors

11. Ah, we need to implement the method

::

    33 get_time() ->
    34   {current_time, unknown}.

So, this implementation doesn't actually return a time, but at least the method is defined, and the message format is clear. This test should pass.

::

    todd@ubuntu:~/temp/time_srv$ sinan test
    starting: depends
    starting: build
    Building /home/todd/temp/time_srv/src/time_srv.erl
    starting: test
    Testing time_srv
    time_srv:  All 2 tests passed.
    time_srv_app:  There were no tests to run.
    time_srv_sup:  There were no tests to run.

A copy of the code at this point is here under time-srv-03.

Add Time Server Functionality
-----------------------------

1. Update the API method in src/time_srv.erl

::

    33 get_time() ->
    34   error_logger:info_msg("[~p] get_time()~n", [?MODULE]),
    35   gen_server:call(?SERVER, get_time).
   
Note: The 'get_tme() api method delegates to gen_server:

 * ?SERVER : we'll use the name of this module, 'time_srv', as the name of the process to forward this call to. Essentially, this means we are expecting 'time_srv' to be a singleton process within this erlang node.
 * 'get_time' : this atom defines the method we are invoking

2. Update the handle_call method in src/time_srv.erl

::

    44 handle_call(Request, From, State) ->
    45   error_logger:info_msg("[~p] handle_call(~p,~p,~p)~n", [?MODULE, Request, From, State]),
    46   case Request of
    47     get_time ->
    48       Now = calendar:local_time(),
    49       NowSecs = calendar:datetime_to_gregorian_seconds(Now),
    50       {reply, {current_time, NowSecs}, State};
    51     _ ->
    52       {reply, {unrecognized, Request}, State}
    53   end.

 * handle_call switches on the Request
 * when handle_call receives a 'get_time' in the Request, it builds a response with the current_time in seconds
 * See the Calendar_ docs for more details on the calendar module

3. Update the test to verify the results

::

    84 get_current_time_test() ->
    85   Before = calendar:local_time(),
    86   BeforeSecs = calendar:datetime_to_gregorian_seconds(Before),
    87   {current_time, ActualSecs} = time_srv:get_time(),
    88   After = calendar:local_time(),
    89   AfterSecs = calendar:datetime_to_gregorian_seconds(After),
    90   %assertions
    91   ?assert(BeforeSecs =< ActualSecs),
    92   ?assert(AfterSecs >= ActualSecs).

 * the key thing here is that we can call the api method on the server, 'time_srv', and the api will use gen_server to create a message and send it to the time_srv process
 * notice the use of asserts, see the Eunit_ Docs for more details


Invoking Methods
----------------
Let's play around with our new service. It might be interesting to examine the differences between invoking the module methods vs calling the gen_server methods.

1. Start the shell

::

    todd@ubuntu:~/temp/time_srv$ sinan build
    starting: depends
    starting: build
    todd@ubuntu:~/temp/time_srv$ sinan shell
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    starting: shell
    Eshell V5.8.2  (abort with ^G)
    1> 

2. Start the service

::

    1> application:start(time_srv).
    ok

3. Woops. Logging isn't turned on...let's try that again. Exit from the shell and start over

::

    todd@ubuntu:~/temp/time_srv$ sinan shell
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    starting: shell
    Eshell V5.8.2  (abort with ^G)
    1> error_logger:tty(true).
    ok
    2> application:start(time_srv).

    =INFO REPORT==== 19-Feb-2011::09:20:18 ===
    [time_srv_app] start(normal,[])
       
    =INFO REPORT==== 19-Feb-2011::09:20:18 ===
    [time_srv_sup] start_link()
       
    =INFO REPORT==== 19-Feb-2011::09:20:18 ===
    [time_srv_sup] init([])
       
    =INFO REPORT==== 19-Feb-2011::09:20:18 ===
    [time_srv] start_link()
       
    =INFO REPORT==== 19-Feb-2011::09:20:18 ===
    [time_srv] init([])
    1> ok
    3> 

4. Invoke the method from the API...

::

    3> time_srv:get_time().

    =INFO REPORT==== 19-Feb-2011::09:21:38 ===
    [time_srv] get_time()
       
    =INFO REPORT==== 19-Feb-2011::09:21:38 ===
    [time_srv] handle_call(get_time,{<0.80.0>,#Ref<0.0.0.397>},[])
    1> {current_time,63465326498}

5. Now let's stop the process an try to invoke this method on a stopped process

::

    5> application:stop(time_srv).

    =INFO REPORT==== 19-Feb-2011::09:23:02 ===
    [time_srv_app] stop([])
       
    =INFO REPORT==== 19-Feb-2011::09:23:02 ===
        application: time_srv
        exited: stopped
        type: temporary
    1> ok
    6> time_srv:get_time().        

    =INFO REPORT==== 19-Feb-2011::09:23:10 ===
    [time_srv] get_time()
    1> ** exception exit: {noproc,{gen_server,call,[time_srv,get_time]}}
         in function  gen_server:call/2

A copy of the code at this point is here under time-srv-04.

Wrap Up
-------

That's it.


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

.. _Episode-02: https://github.com/ToddG/experimental/tree/master/erlang/wilderness/02

.. _Calendar: http://erldocs.com/R14B01/stdlib/calendar.html?i=230

.. _Eunit: http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html
