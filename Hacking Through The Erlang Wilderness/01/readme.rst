=================================================
Hacking Through The Erlang Wilderness : Episode 1
=================================================

.. footer:: Copyright (c) 2011 Todd D. Greenwood-Geer 

:Author: Todd D. Greenwood-Geer
:Date: Tue Feb 13  2011
:Version: 0.2
:Index: Index_ : Listing of all the episodes

-----------------------
Installation
-----------------------

In this demo, I'll walk you through installing erlang and some associated tools. The next demo will walk you through creating your first application using those tools.

1. Get your os in order::

    // I am running these demos on an ubuntu 10.04 guest os in VMWare
    //
    todd@ubuntu:~/temp/app01$ uname -a
    Linux ubuntu 2.6.32-24-generic #43-Ubuntu SMP Thu Sep 16 14:17:33 UTC 2010 i686 GNU/Linux
    //
    //My host is a Macbook
    //
    Model Name:   MacBook
    Model Identifier: MacBook4,1
    Processor Name:   Intel Core 2 Duo
    Processor Speed:  2.1 GHz
    Number Of Processors: 1
    Total Number Of Cores:    2
    L2 Cache: 3 MB
    Memory:   4 GB

Note: I tried running everything on my macbook first. However, I ran into issues and didn't want to bother with sorting them out. So far, running on an Ubuntu VM has been pretty painless.

2. Install erlang and goodies via the package manager::

    // look at all this great stuff we can install !!
    //
    todd@ubuntu:~$ apt-cache search erlang | grep ^erlang
    erlang-doc-html - Erlang/OTP HTML documentation
    erlang-esdl - Erlang bindings to the Simple Direct Media Library
    erlang-esdl-dev - Erlang bindings to the SDL (development files)
    erlang-esdl-doc - Erlang bindings to the SDL (documentation)
    erlang-base - Erlang/OTP virtual machine and base applications
    erlang-crypto - Erlang/OTP cryprographic modules
    erlang-dev - Erlang/OTP development libraries and headers
    erlang-inets - Erlang/OTP Internet clients and servers
    erlang-mnesia - Erlang/OTP distributed relational/object hybrid database
    erlang-os-mon - Erlang/OTP operating system monitor
    erlang-public-key - Erlang/OTP public key infrastructure
    erlang-runtime-tools - Erlang/OTP runtime tracing/debugging tools
    erlang-snmp - Erlang/OTP SNMP applications
    erlang-ssl - Erlang/OTP implementation of SSL
    erlang-syntax-tools - Erlang/OTP modules for handling abstract Erlang syntax trees
    erlang-xmerl - Erlang/OTP XML tools
    erlang - Concurrent, real-time, distributed functional language
    erlang-appmon - Erlang/OTP application monitor
    erlang-asn1 - Erlang/OTP modules for ASN.1 support
    erlang-base-hipe - Erlang/OTP HiPE enabled virtual machine and base applications
    erlang-common-test - Erlang/OTP application for automated testing
    erlang-corba - Erlang/OTP applications for CORBA support
    erlang-debugger - Erlang/OTP application for debugging and testing
    erlang-dialyzer - Erlang/OTP discrepancy analyzer application
    erlang-docbuilder - Erlang/OTP application for building HTML documentation
    erlang-edoc - Erlang/OTP module for generating documentation
    erlang-erl-docgen - Erlang/OTP documentation stylesheets
    erlang-et - Erlang/OTP event tracer application
    erlang-eunit - Erlang/OTP module for unit testing
    erlang-examples - Erlang/OTP application examples
    erlang-gs - Erlang/OTP graphics system
    erlang-ic - Erlang/OTP IDL compiler
    erlang-inviso - Erlang/OTP trace tool
    erlang-manpages - Erlang/OTP manual pages
    erlang-megaco - Erlang/OTP implementation of Megaco/H.248 protocol
    erlang-mode - Erlang major editing mode for Emacs
    erlang-nox - Erlang/OTP applications that don't require X Window System
    erlang-observer - Erlang/OTP application for investigating distributed systems
    erlang-odbc - Erlang/OTP interface to SQL databases
    erlang-parsetools - Erlang/OTP parsing tools
    erlang-percept - Erlang/OTP concurrency profiling tool
    erlang-pman - Erlang/OTP process manager
    erlang-reltool - Erlang/OTP release management tool
    erlang-src - Erlang/OTP applications sources
    erlang-ssh - Erlang/OTP implementation of SSH protocol
    erlang-test-server - Erlang/OTP server for automated application testing
    erlang-toolbar - Erlang/OTP graphical toolbar
    erlang-tools - Erlang/OTP various tools
    erlang-tv - Erlang/OTP table viewer
    erlang-typer - Erlang/OTP code type annotator
    erlang-webtool - Erlang/OTP helper for web-based tools
    erlang-x11 - Erlang/OTP applications that require X Window System

3. Install erlang::

    $ apt-get install erlang

4. Incidentally, I have other erlang stuff installed, too, probably from the erlang package::

    todd@ubuntu:~$ dpkg -l | grep erlang
    ii  erlang-base                      1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP virtual machine and base applications
    ii  erlang-crypto                    1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP cryprographic modules
    ii  erlang-esdl-doc                  1.0.1-3                                  Erlang bindings to the SDL (documentation)
    ii  erlang-manpages                  1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP manual pages
    ii  erlang-mnesia                    1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP distributed relational/object hybrid datab
    ii  erlang-mode                      1:13.b.3-dfsg-2ubuntu2.1                 Erlang major editing mode for Emacs
    ii  erlang-os-mon                    1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP operating system monitor
    ii  erlang-runtime-tools             1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP runtime tracing/debugging tools
    ii  erlang-snmp                      1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP SNMP applications
    ii  erlang-syntax-tools              1:13.b.3-dfsg-2ubuntu2.1                 Erlang/OTP modules for handling abstract Erlang synta

5. Install sinan, faxien, and rebar. By the way, there are great resources showing you how to install these things...

 * Sinan_Faxien_Demo_ : A tutorial that uses sinan and faxien.
 * Erlware_ : Installation instructions that are a bit hard to follow, check out the video above.
 * Basho_Rebar_Demo_ : The basho rebar demo presentation.
 * Rebar_ : Great wiki documentation on rebar. 

6. TODO: flesh out this demo by installing these tools on a fresh vm instance.


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
