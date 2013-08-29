-module(httpd).
-export([start/1,server/1,reqhandler/1]).

start(Port) ->
   spawn(httpd, server, [Port]).

server(Port) ->
   {ok, Lsock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
      {active, false}]),
   serverloop(Lsock).

serverloop(Lsock) ->
   {ok, Sock} = gen_tcp:accept(Lsock),
   spawn(httpd,reqhandler,[Sock]),
   serverloop(Lsock).

reqhandler(Sock) ->
   ReqStr = getreq(Sock),
   [FirstArg, SecondArg | Tail] = string:tokens(ReqStr, " \n\t"),
   if 
      FirstArg =/= "GET" ->
         gen_tcp:send(Sock, list_to_binary("400 Bad Request\r\n"));
      true ->
         {ok, BaseName, _} = regexp:gsub(SecondArg, "/$|^$", 
             "/index.html"),
         {ok, File, _} = regexp:sub(BaseName, "^/+", ""),
         sendfile(Sock, File)
   end,
   gen_tcp:close(Sock).

getreq(Sock) ->
   getreq(Sock, []).

getreq(Sock, OrigStr) ->
   {ok, Pack} = gen_tcp:recv(Sock, 0),
   RecStr = binary_to_list(Pack),
   NewStr = lists:append(OrigStr, RecStr),
   Pos = string:str(NewStr, "\r\n"),
   if 
      Pos =/= 0 ->
         string:substr(NewStr, 1, Pos-1);
      true ->
         getreq(Sock, NewStr)
   end.

sendfile(Sock, Filename) ->
   case file:read_file(Filename) of
      {ok, Binary} ->
         gen_tcp:send(Sock, Binary);
      _ ->
         gen_tcp:send(Sock, list_to_binary("404 Not Found\r\n"))
   end.
