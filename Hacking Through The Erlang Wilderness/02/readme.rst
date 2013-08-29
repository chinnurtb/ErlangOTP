=================================================
Hacking Through The Erlang Wilderness : Episode 2
=================================================

.. footer:: Copyright (c) 2011 Todd D. Greenwood-Geer 

:Author: Todd D. Greenwood-Geer
:Date: Tue Feb 12  2011
:Version: 0.2
:Index: Index_ : Listing of all the episodes

-----------------------
How to Create an App
-----------------------

In this demo, I'm going to use Sinan to create an application skeleton. Later, I'll use Rebar to create individual files. See:

 * Sinan_Faxien_Demo_
 * Basho_Rebar_Demo_
 * Erlware_
 * Rebar_

1. Create A Sample Application Skeleton::

    todd@ubuntu:~/temp$ sinan gen app01
    //
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
    /home/todd/temp/app01/doc created ok.
    /home/todd/temp/app01/bin created ok.
    /home/todd/temp/app01/config created ok.
    /home/todd/temp/app01/ebin created ok.
    /home/todd/temp/app01/src created ok.
    /home/todd/temp/app01/include created ok.
    /home/todd/temp/app01/doc exists ok.
    Would you like a build config? ("y")> y
    Project was created, you should be good to go!

2. Let's take a look at the generated files::

    todd@ubuntu:~/temp$ find app01/
    //
    app01/
    app01/sinan.cfg
    app01/config
    app01/config/sys.config
    app01/src
    app01/src/app01_sup.erl
    app01/src/app01_app.erl
    app01/include
    app01/doc
    app01/bin
    app01/bin/erlware_release_start_helper
    app01/bin/app01
    app01/ebin
    app01/ebin/app01.app
    app01/ebin/overview.edoc

3. Build the app::

    todd@ubuntu:~/temp/app01$ sinan build
    //
    starting: depends
    starting: build
    Building /home/todd/temp/app01/src/app01_sup.erl
    Building /home/todd/temp/app01/src/app01_app.erl

4. To run the app, we first start an erlang shell::

    todd@ubuntu:~/temp/app01$ sinan shell
    //
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
       starting: build
       starting: shell
       Eshell V5.8.2  (abort with ^G)
    1> 

5. Before we start our app, let's see what's running::
    
    1> regs().       
    //
    ** Registered procs on node nonode@nohost **
    Name                  Pid          Initial Call                      Reds Msgs
    alarm_handler         <0.42.0>     gen_event:init_it/6                 28    0
    application_controlle <0.6.0>      erlang:apply/2                    5590    0
    code_server           <0.19.0>     erlang:apply/2                  216335    0
    erl_prim_loader       <0.3.0>      erlang:apply/2                  456499    0
    error_logger          <0.5.0>      gen_event:init_it/6              28163    0
    file_server_2         <0.18.0>     file_server:init/1               47752    0
    global_group          <0.17.0>     global_group:init/1                 60    0
    global_name_server    <0.12.0>     global:init/1                      120    0
    ibrowse               <0.51.0>     ibrowse:init/1                    1059    0
    ibrowse_sup           <0.50.0>     supervisor:ibrowse_sup/1           112    0
    inet_db               <0.15.0>     inet_db:init/1                     207    0
    init                  <0.0.0>      otp_ring0:start/2                 6047    0
    kernel_safe_sup       <0.28.0>     supervisor:kernel/1                124    0
    kernel_sup            <0.10.0>     supervisor:kernel/1              37256    0
    mnesia_checkpoint_sup <0.68.0>     supervisor:mnesia_checkpo           61    0
    mnesia_controller     <0.70.0>     mnesia_controller:init/1           133    0
    mnesia_event          <0.60.0>     gen_event:init_it/6                 36    0
    mnesia_kernel_sup     <0.61.0>     supervisor:mnesia_kernel_          605    0
    mnesia_late_loader    <0.71.0>     mnesia_sp:init_proc/4              161    0
    mnesia_locker         <0.64.0>     mnesia_sp:init_proc/4              111    0
    mnesia_monitor        <0.62.0>     mnesia_monitor:init/1             2113    0
    mnesia_recover        <0.65.0>     mnesia_recover:init/1              320    0
    mnesia_snmp_sup       <0.69.0>     supervisor:mnesia_snmp_su           61    0
    mnesia_subscr         <0.63.0>     mnesia_subscr:init/1                44    0
    mnesia_sup            <0.59.0>     supervisor:mnesia_sup/1            225    0
    mnesia_tm             <0.66.0>     mnesia_sp:init_proc/4             1075    0
    overload              <0.43.0>     overload:init/1                     39    0
    release_handler       <0.44.0>     release_handler:init/1            1152    0
    rex                   <0.11.0>     rpc:init/1                          36    0
    sasl_safe_sup         <0.41.0>     supervisor:sasl/1                  167    0
    sasl_sup              <0.40.0>     supervisor:sasl/1                  158    0
    standard_error        <0.21.0>     erlang:apply/2                       9    0
    standard_error_sup    <0.20.0>     supervisor_bridge:standar           41    0
    timer_server          <0.67.0>     timer:init/1                       767    0
    user                  <0.24.0>     group:server/3                     493    0
    user_drv              <0.23.0>     user_drv:server/2                  910    0
    //
    ** Registered ports on node nonode@nohost **
    Name                  Id              Command                                 
    ok
    2> 


6. Use 'application:start(...)' to start the application under OTP::

    2> application:start(app01).
    //
    {error,{shutdown,{app01_app,start,[normal,[]]}}}
    3> 

7. Our default app returned an error. Well, what do we know about this app?::

    5> m(app01_app).
    Module app01_app compiled: Date: February 13 2011, Time: 01.36
    Compiler options:  [{warn_format,1},
                        debug_info,
                        {outdir,"/home/todd/temp/app01/_build/development/apps/app01-0.0.1/ebin"},
                        strict_record_tests,return_errors,return_warnings,
                        {i,"/home/todd/temp/app01/include"},
                        {i,"/home/todd/temp/app01/_build/development/apps/app01-0.0.1/src"},
                        {i,"/usr/local/erlware/lib/eunit-2.1.6/include"},
                        {i,"/usr/local/erlware/lib/stdlib-1.17.2/include"},
                        {i,"/usr/local/erlware/lib/kernel-2.14.2/include"}]
    Object file: /home/todd/temp/app01/_build/development/apps/app01-0.0.1/ebin/app01_app.beam
    Exports: 
             module_info/0
             module_info/1
             start/2
             stop/1
    ok

8. Well, that was no help. Let's dig into the code and see what's in the default app and see what's there. I think the problem is that the sinan gen target did not fill out all of the supervisor metadata necessary. Let's look at the supervisor file it created::

    #app01/src/app01_sup.erl
    AChild = {'AName', {'AModule', start_link, []},
              Restart, Shutdown, Type, ['AModule']},

9. Change this to::
  
    AChild = {app01, {app01, start_link, []},
           Restart, Shutdown, Type, [app01]},

10. Re-running application:start() gives the same error:

::

    2> application:start(app01).
    //
    {error,{shutdown,{app01_app,start,[normal,[]]}}}
    3> 

11. Let's break out some debugging fu here and add some print statements to tell us what's going on

 * Add some print statements to src/app01_app.erl::

     23 start(_StartType, _StartArgs) ->
     24     io:format("=========================================~n"),
     25     io:format("app01_app:start()~n"),
     26     io:format("=========================================~n"),
     27     case app01_sup:start_link() of
     28         {ok, Pid} ->
     29             {ok, Pid};
     30         Error ->
     31             Error
     32     end.
     33 
     34 %% @private
     35 -spec stop(State::any()) -> ok.
     36 stop(_State) ->
     37     io:format("=========================================~n"),
     38     io:format("app01_app:stop()~n"),
     39     io:format("=========================================~n"),
     40     ok.


 * Start up the shell launch again::

    todd@ubuntu:~/temp/app01$ sinan shell
    //
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
    //
    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    starting: shell
    Eshell V5.8.2  (abort with ^G)
    //
    1> application:start(app01).
    =========================================
    app01: start()
    =========================================
    1> {error,{shutdown,{app01_app,start,[normal,[]]}}}
    2> 

12. Ok, so we can see that the app is entering the app01_app:start() method. And it also seems to be crapping out. We can use the 'regs().' method to see if the app01 process is running (it isn't). We can also put more print statements in to see how the app is starting:

 * Add some print statements to src/app01_sup.erl::

     24 start_link() ->
     25     io:format("=========================================~n"),
     26     io:format("app01_sup:start_link()~n"),
     27     io:format("=========================================~n"),
     28     supervisor:start_link({local, ?SERVER}, ?MODULE, []).
     29 
     38 init([]) ->
     39     io:format("=========================================~n"),
     40     io:format("app01_sup:init()~n"),
     41     io:format("=========================================~n"),

 * Try running the app again::

    todd@ubuntu:~/temp/app01$ sinan shell
    //
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
    //
    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    Building /home/todd/temp/app01/src/app01_sup.erl
    Building /home/todd/temp/app01/src/app01_app.erl
    starting: shell
    Eshell V5.8.2  (abort with ^G)
    1> application:start(app01).
    =========================================
    app01_app: start()
    =========================================
    =========================================
    app01_sup:start_link()
    =========================================
    =========================================
    app01_sup:init()
    =========================================
    1> {error,{shutdown,{app01_app,start,[normal,[]]}}}


13. Ok, so we're definitely crapping out in the init. Call me crazy, but I think we're missing something here. My guess is that we need some piece of code that actually does something. You see, the _app is really just the wrapper to start and stop the app. The _sup is the management code. So what we probably need is something that the init calls into. Let's add a new file to src, called src/app01.erl. We can use Rebar_ to create this file:

 * Install Rebar_ locally::
    
    todd@ubuntu:~/temp/app01$ wget http://bitbucket.org/basho/rebar/downloads/rebar; chmod u+x rebar

 * Create a gen_server file::

    todd@ubuntu:~/temp/app01$ rebar list-templates
    ==> app01 (list-templates)
    Available templates:
            * simplesrv: priv/templates/simplesrv.template (escript)
            * simplenode: priv/templates/simplenode.template (escript)
            * simplemod: priv/templates/simplemod.template (escript)
            * simplefsm: priv/templates/simplefsm.template (escript)
            * simpleapp: priv/templates/simpleapp.template (escript)
            * basicnif: priv/templates/basicnif.template (escript)

    todd@ubuntu:~/temp/app01$ rebar create template=simplesrv

 * Move the file and edit::

    $ mv src/myserver.erl src/app01.erl
    // 
    // change this line from 'myserver' to 'app01'
    //
    -module(myserver).
    //
    -module(app01).

 * The full generated file looks like::

    -module(app01).
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


14. Try to build
    
::

    todd@ubuntu:~/temp/app01/src$ sinan build
    starting: depends
    starting: build
    Module (app01) in file /home/todd/temp/app01/src/app01.erl is not in the module list. Removing from build queue.

15. So, we need to add this file to the 'module list', whatever that is... A quick grep of our project shows a likely candidate:

 * This file seems like it has a module list::

    #ebin/app01.app
    //
    %% This is the application resource file (.app file) for the app01,
    %% application.
    {application, app01,
      [{description, "Your Desc HERE"},
       {vsn, "0.0.1"},
       {modules, [app01_app,
                  app01_sup]},
       {registered,[app01_sup]},
       {applications, [kernel, stdlib]},
       {mod, {app01_app,[]}},
       {start_phases, []}]}.

 * Let's add our app01.erl to the modules entry::

    {modules, [app01_app,
              app01_sup]},
    //
    // becomes
    //
    {modules,[   app01,
                app01_app,
                app01_sup]},


16. Now we're building!
    
::

    todd@ubuntu:~/temp/app01$ sinan build
    //
    starting: depends
    starting: build
    Building /home/todd/temp/app01/src/app01_sup.erl
    Building /home/todd/temp/app01/src/app01_app.erl
    Building /home/todd/temp/app01/src/app01.erl

17. Run the app

::

    todd@ubuntu:~/temp/app01$ sinan shell
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> starting: depends
    starting: build
    starting: shell
    Eshell V5.8.2  (abort with ^G)
    1> application:start(app01).
    =========================================
    app01_app: start()
    =========================================
    =========================================
    app01_sup:start_link()
    =========================================
    =========================================
    app01_sup:init()
    =========================================
    1> ok

18. Cool. We finally have an app that at least doesn't crap out. Does our app show up in the registry? 

::

    2> regs().
    //
    ** Registered procs on node nonode@nohost **
    Name                  Pid          Initial Call                      Reds Msgs
    alarm_handler         <0.42.0>     gen_event:init_it/6                 28    0
    app01                 <0.100.0>    app01:init/1                        26    0
    app01_sup             <0.99.0>     supervisor:app01_sup/1             150    0
    ...

19. Yes it does. 

20. So, in the next episode, I'll implement some actual functionality for this app.

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

.. _Sinan_Faxien_Demo: http://www.youtube.com/watch?v=XI7S2NwFPOE

.. _Basho_Rebar_Demo: http://blog.basho.com/category/rebar/

.. _Erlware: http://erlware.com/

.. _Rebar: https://bitbucket.org/basho/rebar/wiki/GettingStarted

.. _Index: https://github.com/ToddG/experimental/tree/master/erlang/wilderness
