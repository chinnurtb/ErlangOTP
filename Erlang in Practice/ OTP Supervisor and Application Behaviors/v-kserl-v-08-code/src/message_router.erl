-module(message_router).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([send_chat_message/2, register_nick/2, unregister_nick/1, shutdown/0]).

%% Client functions

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

send_chat_message(Addressee, MessageBody) ->
  gen_server:call({global, ?SERVER}, {send_chat_msg, Addressee, MessageBody}).

register_nick(ClientName, ClientPid) ->
  gen_server:call({global, ?SERVER}, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
  io:format("Unregistering nickname: ~p~n", [ClientName]),
  gen_server:call({global, ?SERVER}, {unregister_nick, ClientName}).

shutdown() ->
  gen_server:cast({global, ?SERVER}, stop).

%% gen_server callback functions

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  {ok, dict:new()}.

handle_call({send_chat_msg, ClientName, MessageBody}, _From, Clients) ->
  case dict:find(ClientName, Clients) of
    {ok, ClientPid} ->
      ClientPid ! {printmsg, MessageBody};
    error ->
      message_store:save_message(ClientName, MessageBody),
      io:format("Archived message for ~p~n", [ClientName])
  end,
  {reply, ok, Clients};

handle_call({register_nick, ClientName, ClientPid}, _From, Clients) ->
  Messages = message_store:find_messages(ClientName),
  lists:foreach(fun(Msg) -> ClientPid ! {printmsg, Msg} end, Messages),
  {reply, ok, dict:store(ClientName, ClientPid, Clients)};


handle_call({unregister_nick, ClientName}, _From, Clients) ->
  UpdatedClients = case dict:find(ClientName, Clients) of
		    {ok, ClientPid} ->
		      ClientPid ! stop,
		      dict:erase(ClientName, Clients);
		    error ->
		      io:format("Error! Unknown client: ~p~n", [ClientName]),
		      Clients
		  end,
  {reply, ok, UpdatedClients};


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
