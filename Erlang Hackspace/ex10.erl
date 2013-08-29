-module(ex10).

-behaviour(gen_server).

%% API
-export([start_link/0, double/1, square/1, stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {ops, wtfs}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

double(N) -> gen_server:call(?MODULE, {double, N}).
square(N) -> gen_server:call(?MODULE, {square, N}).
stats()   -> gen_server:call(?MODULE, stats).

%% gen_server callbacks
init([]) ->
    {ok, #state{ops=0, wtfs=0}}.

handle_call(stats, _From, State=#state{ops=Ops, wtfs=Wtfs}) ->
    {reply, {Ops, Wtfs}, State}; 

handle_call({double, N}, _From, State=#state{ops=Ops}) ->
    {reply, N*2, State#state{ops=Ops+1}};

handle_call({square, N}, _From, State=#state{ops=Ops}) ->
    {reply, N*N, State#state{ops=Ops+1}};

handle_call({_, _N}, _From, State=#state{ops=Wtfs}) ->
    {reply, wtf, State#state{wtfs=Wtfs+1}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.