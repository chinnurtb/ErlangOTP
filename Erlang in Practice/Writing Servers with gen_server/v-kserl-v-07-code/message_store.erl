-module(message_store).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, ?MODULE).

-record(chat_message,
	{addressee,
	 body,
	 created_on}).

%% API
-export([start_link/0, save_message/2, find_messages/1, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%% Client API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save_message(Addressee, Body) ->
  gen_server:call(?SERVER, {save_msg, Addressee, Body}).

find_messages(Addressee) ->
  case gen_server:call(?SERVER, {find_msgs, Addressee}) of
    {ok, Messages} ->
      Messages
  end.

shutdown() ->
  gen_server:call(?SERVER, stop).

%% gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  init_store(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({save_msg, Addressee, Body}, _From, State) ->
  store_message(Addressee, Body),
  {reply, ok, State};

handle_call({find_msgs, Addressee}, _From, State) ->
  Messages = get_messages(Addressee),
  {reply, {ok, Messages}, State};

handle_call(stop, _From, State) ->
  mnesia:stop(),
  {stop, normal, State};

handle_call(_Request, _From, State) ->
  {reply, ignored_message, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
store_message(Addressee, Body) ->
  F = fun() ->
	  {_, CreatedOn, _} = erlang:now(),
	  mnesia:write(#chat_message{addressee=Addressee, body=Body, created_on=CreatedOn}) end,
  mnesia:transaction(F).

get_messages(Addressee) ->
  F = fun() ->
	  Query = qlc:q([M || M <- mnesia:table(chat_message),
			      M#chat_message.addressee =:= Addressee]),
	  Results = qlc:e(Query),
	  delete_messages(Results),
	  lists:map(fun(Msg) -> Msg#chat_message.body end, Results) end,
  {atomic, Messages} = mnesia:transaction(F),
  Messages.

delete_messages(Messages) ->
  F = fun() ->
	  lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages) end,
  mnesia:transaction(F).

init_store() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(chat_message, type)
  catch
    exit: _ ->
      mnesia:create_table(chat_message, [{attributes, record_info(fields, chat_message)},
					 {type, bag},
					 {disc_copies, [node()]}])
  end.
