%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(text_resource).
-export([init/1, to_text/2, content_types_provided/2]).
-import(string).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"text/plain",to_text}], ReqData, Context}.


%%to_html(ReqData, State) ->
%%    {"<html><body>Hello from service </body></html>", ReqData, State}.

to_text(ReqData, State) ->
    {"<html><body>Text Resource</body></html>", ReqData, State}.
