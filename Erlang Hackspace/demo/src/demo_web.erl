%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for demo.

-module(demo_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "comet/" ++ Id ->
                    ProcName = list_to_atom(Id),
                    erlang:register(ProcName, self()),
                    Resp = Req:ok( { "text/html", [], chunked } ),
                    Resp:write_chunk( io_lib:format("Welcome, ~s", [Id]) ),
                    feed(Resp);

                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

feed(Resp) ->
    receive
        X when is_list(X) ->
            Resp:write_chunk(X),
            feed(Resp)
    end.




get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
