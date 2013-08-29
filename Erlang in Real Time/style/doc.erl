-module(doc).
-author('Maurice Castro').
-copyright('Copyright (c) 1998').
-vsn('$Id: doc.erl,v 1.4 1998/06/22 02:33:07 maurice Exp $').
-modified('Mon Jun 22 08:38:16 EST 1998').
-modified_by('maurice@serc').
-modified('Mon Jun 22 12:17:35 EST 1998').
-modified_by('maurice@serc').

-export([start/0, start/1, p/1, v/1, msglp/2]).

%%% --------------------------------------------------------------
%%% This module illustrates documentation conventions described in 
%%% `Erlang in Real Time' by Maurice Castro
%%% It implements a Counting Semaphore as defined by Hwang K,
%%% `Advanced Computer Architecure' McGraw Hill, 1993, p638.
%%% Warning this code assumes no messages are lost.
%%% --------------------------------------------------------------

%% ---------------------------------------------------------------
%% This is the special case of a binary semaphore, use general
%% start routine to perform start.
%% ---------------------------------------------------------------

start() ->
	start(1).

%% ---------------------------------------------------------------
%% The start/1 function starts a server and returns its pid.
%% This is the general case 
%% ---------------------------------------------------------------

start(N) ->
	% spawn the message loop with initial data of N 
	spawn(doc, msglp, [N, []]).


%% ---------------------------------------------------------------
%% P(s): if s > 0, then s = s - 1; else put in wait queue
%% ---------------------------------------------------------------

p(S) ->
	S ! {semaphore, p, self()},	% cont requires process name
	% wait for a continue message to indicate exiting queue
	receive
		{semaphore, cont} -> 
			true
	end.

%% ---------------------------------------------------------------
%% V(s): if wait queue not empty, wake up one; else s = s + 1
%% ---------------------------------------------------------------

v(S) ->
	S ! {semaphore, v}.	% Server handles v

%% ---------------------------------------------------------------
%% The msglp function handles   cases: 
%% ---------------------------------------------------------------

msglp(S, L) ->
	{NewS, NewL} = receive
		{semaphore, p, Pid} ->
			if 
				% P(s): if s > 0, then s = s - 1; 
				S > 0 ->
					Pid ! {semaphore, cont}, 
					{S - 1, L};
				true ->
					% else put in wait queue
					{S, [Pid, L]}
			end;
		% V(s): if wait queue not empty, 
		{semaphore, v} when length(L) =/= 0 ->
			[H|T] = L,
			% wake up one; 
			H ! {semaphore, cont},
			{S, T};
		% if the list is empty on a v 
		{semaphore, v} ->
			% else s = s + 1
			{S+1, L}
	end,
	msglp(NewS, NewL).
