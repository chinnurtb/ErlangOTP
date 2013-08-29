=================================================
Hacking Through The Erlang Wilderness : Episode 6
=================================================

.. footer:: Copyright (c) 2011 Todd D. Greenwood-Geer 

:Author: Todd D. Greenwood-Geer
:Date: Tue March 22,  2011
:Version: 0.1
:Index: Index_ : Listing of all the episodes


---------------------------------------------------
How to Create an Mnesia Based Application (Part 1)
---------------------------------------------------

Goal
----

Load a dataset into Amnesia and present an interface to query the dataset. For this example, we'll load up the historical DJI trade data. The API over this data set will be:

* Min/Max/Average/Median over a given date range
* Full records over a given date range 

In Part 2, we'll surface this API as a rest endpoint.

In Part 3, we'll generate sparklines from this data, via the rest API. 


Launch Erlang with Mnesia started
---------------------------------

1. start erlang

::

   user@erlang32:~$ erl -mnesia dir '"~/temp/mnesia_data"' -name mynode
   Erlang R13B04 (erts-5.7.5) [source] [rq:1] [async-threads:0] [kernel-poll:false]

   Eshell V5.7.5  (abort with ^G)
   (mynode@erlang32.localdomain)1> 
   
2. create database schema

::

   Eshell V5.7.5  (abort with ^G)
   (mynode@erlang32.localdomain)1> mnesia:create_schema([node()]).
   {error,{"Cannot create Mnesia dir",
           "/home/user/temp/mnesia_data",enoent}}

3. failed, apparently mnesia doesn't create the data dir for us

::

    user@erlang32:~$ mkdir -p temp/mnesia_data
    user@erlang32:~$ tree temp
    temp
    └── mnesia_data

4. try to create database schema again

::

   (mynode@erlang32.localdomain)2> mnesia:create_schema([node()]).
   ok

5. start the db and get some stats

::

   (mynode@erlang32.localdomain)3> mnesia:start().
   ok
   (mynode@erlang32.localdomain)4> mnesia:info().
   ---> Processes holding locks <--- 
   ---> Processes waiting for locks <--- 
   ---> Participant transactions <--- 
   ---> Coordinator transactions <---
   ---> Uncertain transactions <--- 
   ---> Active tables <--- 
   schema         : with 1        records occupying 396      words of mem
   ===> System info in version "4.4.13", debug level = none <===
   opt_disc. Directory "/home/user/temp/mnesia_data" is used.
   use fallback at restart = false
   running db nodes   = ['mynode@erlang32.localdomain']
   stopped db nodes   = [] 
   master node tables = []
   remote             = []
   ram_copies         = []
   disc_copies        = [schema]
   disc_only_copies   = []
   [{'mynode@erlang32.localdomain',disc_copies}] = [schema]
   2 transactions committed, 0 aborted, 0 restarted, 0 logged to disc
   0 held locks, 0 in queue; 0 local transactions, 0 remote
   0 transactions waits for other nodes: []
   ok

Create a Project
----------------

1. Use rebar to create app skeleton

::

    user@erlang32:~ $ cd projects
    user@erlang32:~/projects/$ mkdir stock
    user@erlang32:~/projects/$ cd stock
    user@erlang32:~/projects/stock$ rebar create-app appid=stock
    user@erlang32:~/projects/stock$ tree
    .
    └── src
        ├── stock_app.erl
        ├── stock.app.src
        └── stock_sup.erl


2. Download data as csv::

    user@erlang32:~/projects/stock$ mkdir data
    user@erlang32:~/projects/stock$ cd data

See http://finance.yahoo.com/q/hp?s=^DJI+Historical+Prices

::
    $ wget http://ichart.finance.yahoo.com/table.csv?s=%5EDJI&d=2&e=23&f=2011&g=d&a=9&b=1&c=1928&ignore=.csv

Rename downloaded file to dji.csv

3. Summary

::

    user@erlang32:~/projects/stock$ tree
    .
    ├── data
    │   └── dji.csv
    └── src
        ├── stock_app.erl
        ├── stock.app.src
        └── stock_sup.erl


Input Dataset
-------------

1. Here's a snippet of our dataset, which has >20k records

::

   Date,Open,High,Low,Close,Volume,Adj Close
   2011-03-22,12036.37,12096.01,11965.38,12018.63,3576550000,12018.63
   2011-03-21,11860.11,12117.88,11860.11,12036.53,4223730000,12036.53
   2011-03-18,11777.23,11971.14,11777.23,11858.52,4685500000,11858.52
   2011-03-17,11614.82,11842.55,11614.82,11774.59,4134950000,11774.59
   2011-03-16,11854.13,11862.08,11548.14,11613.30,5833000000,11613.30
   2011-03-15,11988.69,11988.69,11648.50,11855.42,5201400000,11855.42
   2011-03-14,12042.13,12058.44,11873.43,11993.16,4050370000,11993.16
   2011-03-11,11976.96,12087.01,11936.32,12044.40,3740400000,12044.40
   2011-03-10,12211.43,12211.43,11924.48,11984.61,4723020000,11984.61
   2011-03-09,12211.16,12293.74,12106.68,12213.09,3709520000,12213.09
   2011-03-08,12085.80,12276.37,12039.02,12214.38,4531420000,12214.38
   2011-03-07,12171.09,12268.87,12025.51,12090.03,3964730000,12090.03
   2011-03-04,12258.88,12306.26,12056.81,12169.88,4223740000,12169.88

2. The data has this form

::

   Date,Open,High,Low,Close,Volume,Adj Close


3. Let's create a record to hold this data (src/create_tables.erl)

::
    
    -module(create_tables).
    -export([init_tables/0]).

    -record(entry, {stock,date,open,high,low,close,volume,adjclose}).

    init_tables() ->
        mnesia:create_table(entry,
            [{type,bag}, {attributes, record_info(fields, entry)}]).

.. Note:: Apparently the table *must* use the same atom as the record, in this case 'entry'. I originally tried naming the table with a different atom, thinking that the mnesia:create_table() method would then link the record to the table... that did not work.

4. Compile and re-launch erlang

::

    user@erlang32:~/projects/stock$ rebar compile
    ==> stock (compile)
    Compiled src/stock_app.erl
    Compiled src/create_tables.erl
    Compiled src/stock_sup.erl

    user@erlang32:~/projects/stock$ erl -mnesia dir '"/home/user/temp/mnesia_data"' -name mynode -pa ebin/
    Erlang R13B04 (erts-5.7.5) [source] [rq:1] [async-threads:0] [kernel-poll:false]

    Eshell V5.7.5  (abort with ^G)

5. Create the tables via the init_tables() method

::

    (mynode@erlang32.localdomain)4> create_tables:init_tables().
    {atomic,ok}

6. Check out the database

::

    (mynode@erlang32.localdomain)5> mnesia:info().              
    ---> Processes holding locks <--- 
    ---> Processes waiting for locks <--- 
    ---> Participant transactions <--- 
    ---> Coordinator transactions <---
    ---> Uncertain transactions <--- 
    ---> Active tables <--- 
    stock_entry    : with 0        records occupying 286      words of mem
    schema         : with 2        records occupying 518      words of mem
    ===> System info in version "4.4.13", debug level = none <===
    opt_disc. Directory "/home/user/temp/mnesia_data" is used.
    use fallback at restart = false
    running db nodes   = ['mynode@erlang32.localdomain']
    stopped db nodes   = [] 
    master node tables = []
    remote             = []
    ram_copies         = [stock_entry]
    disc_copies        = [schema]
    disc_only_copies   = []
    [{'mynode@erlang32.localdomain',disc_copies}] = [schema]
    [{'mynode@erlang32.localdomain',ram_copies}] = [stock_entry]
    3 transactions committed, 0 aborted, 0 restarted, 1 logged to disc
    0 held locks, 0 in queue; 0 local transactions, 0 remote
    0 transactions waits for other nodes: []
    ok

7. The stock_entry table was created in ram

::

    ram_copies         = [stock_entry]

8. Add a load_data() method to create_tables.erl

::

     11 load_data(FileName, StockName) ->
     12     {ok, FileDescriptor} = file:open(FileName, [read]),
     13     %error_logger:info_msg("load_data: FileName=~p, StockName=~p, FileDescriptor=~p~n", [FileName, StockName, FileDescriptor]),
     14     %discard first line
     15     file:read_line(FileDescriptor),
     16     process_file(FileDescriptor, StockName).
     17 
     18 process_file(FD, SN) ->
     19     case file:read_line(FD) of
     20         {ok, Line} ->
     21             %error_logger:info_msg("process_file: Line=~p", [Line]),
     22             parse_line(Line, SN),
     23             process_file(FD, SN);
     24         _ ->
     25             {done}
     26     end.
     27 
     28 parse_line(Line, StockName) ->
     29     [Date,Open,High,Low,Close,Volume,AdjClose] = string:tokens(Line, ","),
     30     Entry = #entry{stock = StockName, date = Date, open = Open, high = High, low = Low, close = Close, volume = Volume, adjclose = AdjClose},
     31     error_logger:info_msg("parse_line: Entry=~p", [Entry]),
     32     insert_in_database(Entry).
     33 
     34 insert_in_database(NewEntry) ->
     35     error_logger:info_msg("insert_in_database: NewEntry=~p", [NewEntry]),
     36     mnesia:transaction(fun() -> mnesia:write(NewEntry) end).

9. Start up the shell

::

    user@erlang32:~/projects/stock$ erl -mnesia dir '"/home/user/temp/mnesia_data"' -name mynode -pa ebin/

10. Compile and load the data

::

    (mynode@erlang32.localdomain)33> c("src/create_tables").
    (mynode@erlang32.localdomain)33> create_tables:init_tables().                           
    (mynode@erlang32.localdomain)33> create_tables:load_data("./data/short_dji.csv", "dji").

10. Debug output

::

    =INFO REPORT==== 22-Mar-2011::23:44:37 ===
    parse_line: Entry={entry,"dji","2011-03-22","12036.37","12096.01","11965.38",
                             "12018.63","3576550000","12018.63\n"}
    =INFO REPORT==== 22-Mar-2011::23:44:37 ===
    insert_in_database: NewEntry={entry,"dji","2011-03-22","12036.37","12096.01",
                                        "11965.38","12018.63","3576550000",
                                        "12018.63\n"}
    =INFO REPORT==== 22-Mar-2011::23:44:37 ===
    parse_line: Entry={entry,"dji","2011-03-21","11860.11","12117.88","11860.11",
                             "12036.53","4223730000","12036.53\n"}
    =INFO REPORT==== 22-Mar-2011::23:44:37 ===
    insert_in_database: NewEntry={entry,"dji","2011-03-21","11860.11","12117.88",
                                        "11860.11","12036.53","4223730000",
                                        "12036.53\n"}
    =INFO REPORT==== 22-Mar-2011::23:44:37 ===
    parse_line: Entry={entry,"dji","2011-03-18","11777.23","11971.14","11777.23",
                             "11858.52","4685500000","11858.52\n"}
    =INFO REPORT==== 22-Mar-2011::23:44:37 ===
    insert_in_database: NewEntry={entry,"dji","2011-03-18","11777.23","11971.14",
                                        "11777.23","11858.52","4685500000",
                                        "11858.52\n"}

11. Verify that the data is in the db

::

    (mynode@erlang32.localdomain)32> mnesia:dirty_read(entry, "dji").                       
    [{entry,"dji","2011-03-22","12036.37","12096.01","11965.38",
            "12018.63","3576550000","12018.63\n"},
     {entry,"dji","2011-03-21","11860.11","12117.88","11860.11",
            "12036.53","4223730000","12036.53\n"},
     {entry,"dji","2011-03-18","11777.23","11971.14","11777.23",
            "11858.52","4685500000","11858.52\n"},
     {entry,"dji","2011-03-17","11614.82","11842.55","11614.82",
            "11774.59","4134950000","11774.59\n"},
     {entry,"dji","2011-03-16","11854.13","11862.08","11548.14",
            "11613.30","5833000000","11613.30\n"},
     {entry,"dji","2011-03-15","11988.69","11988.69","11648.50",
            "11855.42","5201400000","11855.42\n"},
     {entry,"dji","2011-03-14","12042.13","12058.44","11873.43",
            "11993.16","4050370000","11993.16\n"},
     {entry,"dji","2011-03-11","11976.96","12087.01","11936.32",
            "12044.40","3740400000","12044.40\n"},
     {entry,"dji","2011-03-10","12211.43","12211.43","11924.48",
            "11984.61","4723020000","11984.61\n"}]

TODO: remove the \n in the file parsing.

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
