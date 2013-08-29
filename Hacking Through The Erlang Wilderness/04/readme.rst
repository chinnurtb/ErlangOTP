=================================================
Hacking Through The Erlang Wilderness : Episode 4
=================================================

.. footer:: Copyright (c) 2011 Todd D. Greenwood-Geer 

:Author: Todd D. Greenwood-Geer, Sean Jensen-Grey
:Date: Tue March 15  2011
:Version: 0.2
:Index: Index_ : Listing of all the episodes


----------------------------------------
How to Create a Webmachine Demo App
----------------------------------------

Goal
----

Create a basic Webmachine demo application that returns 'hello world'.

Example::

    http://erlang-server:8000

This should return 'hello world' in the response.


Construct the VM
----------------

1. See Episode-00_.

2. Add the machine IP to your host's /etc/hosts file::

    #erlang32
    192.168.87.10 erlang32

*Assuming that the erlang guest vm is 192.168.87.10.*

3. Ssh to the guest vm::

    user@erlang32$ ssh user@user@erlang32

4. Set the hostname on this guest vm::

    user@erlang32$ sudo hostname user@erlang32

5. Install vim and tree::

    user@erlang32$ sudo apt-get intstall vim tree

6. Edit the hostname::

    user@erlang32$ sudo vim /etc/hostname

7. Set this to 'user@erlang32', too.


Pull down erlang resources
--------------------------


1. Install erlang resources::

    user@erlang32$ apt-get install erlang erlang-dev git-core

2. Sync webmachine from github repo::

    user@erlang32$ git clone https://github.com/basho/webmachine.git

    user@erlang32:~$  git clone https://github.com/basho/webmachine.git
    Initialized empty Git repository in /home/user/webmachine/.git/
    remote: Counting objects: 1146, done.
    remote: Compressing objects: 100% (447/447), done.
    remote: Total 1146 (delta 709), reused 1066 (delta 668)
    Receiving objects: 100% (1146/1146), 1.17 MiB | 69 KiB/s, done.
    Resolving deltas: 100% (709/709), done.

3. Build webmachine::

    user@erlang32$ cd webmachine

    user@erlang32:~/webmachine$ make
    ==> webmachine (get-deps)
    Pulling mochiweb from {git,"git://github.com/basho/mochiweb",
       {tag,"mochiweb-1.7.1"}}
       Initialized empty Git repository in /home/user/webmachine/deps/mochiweb/.git/
       ==> mochiweb (get-deps)
       ==> mochiweb (compile)
       Compiled src/mochiweb_skel.erl
       Compiled src/mochitemp.erl
       Compiled src/mochijson.erl
       Compiled src/mochilists.erl
       Compiled src/mochifmt_std.erl
       Compiled src/mochifmt.erl
       Compiled src/mochiweb_request.erl
       Compiled src/mochilogfile2.erl
       Compiled src/mochiweb_socket.erl
       Compiled src/mochihex.erl
       Compiled src/mochiweb_http.erl
       Compiled src/mochiweb_charref.erl
       Compiled src/mochiweb_app.erl
       Compiled src/mochiweb_html.erl
       Compiled src/mochiweb_echo.erl
       Compiled src/mochinum.erl
       Compiled src/mochifmt_records.erl
       Compiled src/mochijson2.erl
       Compiled src/mochiweb_socket_server.erl
       Compiled src/reloader.erl
       Compiled src/mochiweb_acceptor.erl
       Compiled src/mochiweb_util.erl
       Compiled src/mochiweb_sup.erl
       Compiled src/mochiweb_io.erl
       Compiled src/mochiutf8.erl
       Compiled src/mochiweb_mime.erl
       Compiled src/mochiglobal.erl
       Compiled src/mochiweb_cover.erl
       Compiled src/mochiweb_multipart.erl
       Compiled src/mochiweb_headers.erl
       Compiled src/mochiweb_response.erl
       Compiled src/mochiweb_cookies.erl
       Compiled src/mochiweb.erl
       ==> webmachine (compile)
       Compiled src/webmachine_resource.erl
       Compiled src/webmachine_error_handler.erl
       Compiled src/webmachine_router.erl
       Compiled src/webmachine_mochiweb.erl
       Compiled src/webmachine_deps.erl
       Compiled src/webmachine_dispatcher.erl
       Compiled src/webmachine_multipart.erl
       Compiled src/webmachine_sup.erl
       Compiled src/wmtrace_resource.erl
       Compiled src/webmachine_util.erl
       Compiled src/webmachine_logger.erl
       Compiled src/webmachine_perf_logger.erl
       Compiled src/wrq.erl
       Compiled src/webmachine.erl
       Compiled src/webmachine_app.erl
       Compiled src/webmachine_request.erl
       Compiled src/webmachine_decision_core.erl


.. Note: This pulls down mochiweb, but has a broken rebar

::

    user@erlang32:~/webmachine$ diff rebar deps/mochiweb/rebar
    Binary files rebar and deps/mochiweb/rebar differ


4. Copy the rebar from ./webmachine to deps/mochiweb::

    user@erlang32:~/webmachine$ cp rebar deps/mochiweb/rebar

3. Now try building again::

    user@erlang32:~/webmachine$ make

    ==> mochiweb (get-deps)
    ==> webmachine (get-deps)
    ==> mochiweb (compile)
    ==> webmachine (compile)

This compiles mochiweb AND webmachine.


Create Application from Template
--------------------------------

See http://webmachine.basho.com/quickstart.html

1. Create new webmachine app::

    user@erlang32:~$ mkdir projects && cd projects
    
    user@erlang32:~/projects$ ~/webmachine/scripts/new_webmachine.sh mywebdemo ./webmachine
    ==> priv (create)
    Writing /home/user/projects/webmachine/mywebdemo/README
    Writing /home/user/projects/webmachine/mywebdemo/Makefile
    Writing /home/user/projects/webmachine/mywebdemo/rebar.config
    Writing /home/user/projects/webmachine/mywebdemo/rebar
    Writing /home/user/projects/webmachine/mywebdemo/start.sh
    Writing /home/user/projects/webmachine/mywebdemo/src/mywebdemo.app.src
    Writing /home/user/projects/webmachine/mywebdemo/src/mywebdemo.erl
    Writing /home/user/projects/webmachine/mywebdemo/src/mywebdemo_app.erl
    Writing /home/user/projects/webmachine/mywebdemo/src/mywebdemo_sup.erl
    Writing /home/user/projects/webmachine/mywebdemo/src/mywebdemo_resource.erl
    Writing /home/user/projects/webmachine/mywebdemo/priv/dispatch.conf

2. Examine files::

    user@erlang32:~/projects$ cd webmachine/mywebdemo/
    user@erlang32:~/projects/webmachine/mywebdemo$ ls
    deps  Makefile  priv  README  rebar  rebar.config  src  start.sh

3. Build::

    user@erlang32:~/projects/webmachine/mywebdemo$ make

    ==> mywebdemo (get-deps)
    Pulling webmachine from {git,"git://github.com/basho/webmachine","HEAD"}
    Initialized empty Git repository in /home/user/projects/webmachine/mywebdemo/deps/webmachine/.git/
    ==> webmachine (get-deps)
    Pulling mochiweb from {git,"git://github.com/basho/mochiweb",
       {tag,"mochiweb-1.7.1"}}
       Initialized empty Git repository in /home/user/projects/webmachine/mywebdemo/deps/mochiweb/.git
    ==> mochiweb (get-deps)
    ==> mochiweb (compile)
    Compiled src/mochiweb_skel.erl
    Compiled src/mochitemp.erl
    Compiled src/mochijson.erl
    Compiled src/mochilists.erl
    Compiled src/mochifmt_std.erl
    Compiled src/mochifmt.erl
    Compiled src/mochiweb_request.erl
    Compiled src/mochilogfile2.erl
    Compiled src/mochiweb_socket.erl
    Compiled src/mochihex.erl
    Compiled src/mochiweb_http.erl
    Compiled src/mochiweb_charref.erl
    Compiled src/mochiweb_app.erl
    Compiled src/mochiweb_html.erl
    Compiled src/mochiweb_echo.erl
    Compiled src/mochinum.erl
    Compiled src/mochifmt_records.erl
    Compiled src/mochijson2.erl
    Compiled src/mochiweb_socket_server.erl
    Compiled src/reloader.erl
    Compiled src/mochiweb_acceptor.erl
    Compiled src/mochiweb_util.erl
    Compiled src/mochiweb_sup.erl
    Compiled src/mochiweb_io.erl
    Compiled src/mochiutf8.erl
    Compiled src/mochiweb_mime.erl
    Compiled src/mochiglobal.erl
    Compiled src/mochiweb_cover.erl
    Compiled src/mochiweb_multipart.erl
    Compiled src/mochiweb_headers.erl
    Compiled src/mochiweb_response.erl
    Compiled src/mochiweb_cookies.erl
    Compiled src/mochiweb.erl
    ==> webmachine (compile)
    Compiled src/webmachine_resource.erl
    Compiled src/webmachine_error_handler.erl
    Compiled src/webmachine_router.erl
    Compiled src/webmachine_mochiweb.erl
    Compiled src/webmachine_deps.erl
    Compiled src/webmachine_dispatcher.erl
    Compiled src/webmachine_multipart.erl
    Compiled src/webmachine_sup.erl
    Compiled src/wmtrace_resource.erl
    Compiled src/webmachine_util.erl
    Compiled src/webmachine_logger.erl
    Compiled src/webmachine_perf_logger.erl
    Compiled src/wrq.erl
    Compiled src/webmachine.erl
    Compiled src/webmachine_app.erl
    Compiled src/webmachine_request.erl
    Compiled src/webmachine_decision_core.erl
    ==> mywebdemo (compile)
    Compiled src/mywebdemo_app.erl
    Compiled src/mywebdemo.erl
    Compiled src/mywebdemo_sup.erl
    Compiled src/mywebdemo_resource.erl

4. Run::

    user@erlang32:~/projects/webmachine/mywebdemo$ ./start.sh 

    Erlang R13B03 (erts-5.7.4) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,sasl_safe_sup}
                 started: [{pid,<0.36.0>},
                           {name,alarm_handler},
                           {mfa,{alarm_handler,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,2000},
                           {child_type,worker}]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,sasl_safe_sup}
                 started: [{pid,<0.37.0>},
                           {name,overload},
                           {mfa,{overload,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,2000},
                           {child_type,worker}]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,sasl_sup}
                 started: [{pid,<0.35.0>},
                           {name,sasl_safe_sup},
                           {mfa,
                               {supervisor,start_link,
                                   [{local,sasl_safe_sup},sasl,safe]}},
                           {restart_type,permanent},
                           {shutdown,infinity},
                           {child_type,supervisor}]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,sasl_sup}
                 started: [{pid,<0.38.0>},
                           {name,release_handler},
                           {mfa,{release_handler,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,2000},
                           {child_type,worker}]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
             application: sasl
              started_at: nonode@nohost

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,kernel_safe_sup}
                 started: [{pid,<0.43.0>},
                           {name,timer_server},
                           {mfa,{timer,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,1000},
                           {child_type,worker}]
    Eshell V5.7.4  (abort with ^G)

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,crypto_sup}
                 started: [{pid,<0.50.0>},
                           {name,crypto_server},
                           {mfa,{crypto_server,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,2000},
                           {child_type,worker}]
    1> 
    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
             application: crypto
              started_at: nonode@nohost

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
             application: mochiweb
              started_at: nonode@nohost
    ** Found 0 name clashes in code paths 

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,webmachine_sup}
                 started: [{pid,<0.61.0>},
                           {name,webmachine_router},
                           {mfa,{webmachine_router,start_link,[]}},
                           {restart_type,permanent},
                           {shutdown,5000},
                           {child_type,worker}]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
             application: webmachine
              started_at: nonode@nohost
    opening log file: "priv/log/access.log.2011_03_16_03"

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,webmachine_sup}
                 started: [{pid,<0.67.0>},
                           {name,webmachine_logger},
                           {mfa,{webmachine_logger,start_link,["priv/log"]}},
                           {restart_type,permanent},
                           {shutdown,5000},
                           {child_type,worker}]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
              supervisor: {local,mywebdemo_sup}
                 started: [{pid,<0.68.0>},
                           {name,webmachine_mochiweb},
                           {mfa,
                               {webmachine_mochiweb,start,
                                   [[{ip,"0.0.0.0"},
                                     {port,8000},
                                     {log_dir,"priv/log"},
                                     {dispatch,[{[],mywebdemo_resource,[]}]}]]}},
                           {restart_type,permanent},
                           {shutdown,5000},
                           {child_type,worker}]

    =PROGRESS REPORT==== 15-Mar-2011::20:05:16 ===
             application: mywebdemo
              started_at: nonode@nohost

5. Open in browser::

    host$ open http://erlang32:8000

.. image:: https://github.com/ToddG/experimental/raw/master/erlang/wilderness/04/images/screen01.png


Done
----


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

.. _Calendar: http://erldocs.com/R14B01/stdlib/calendar.html?i=230

.. _Eunit: http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html
