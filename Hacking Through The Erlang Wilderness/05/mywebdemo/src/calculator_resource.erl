%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(calculator_resource).
-export([init/1, to_text/2, content_types_provided/2]).
-import(string).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"text/plain",to_text}], ReqData, Context}.


%%to_html(ReqData, State) ->
%%    {"<html><body>Hello from service </body></html>", ReqData, State}.

%%to_text(ReqData, State) ->
%%    {"<html><body>Text Hello From Service</body></html>", ReqData, State}.

add_tokens(Tokens) ->
    {A,_} = string:to_integer(lists:nth(1,Tokens)),
    {B,_} = string:to_integer(lists:nth(2,Tokens)),
    string:join([integer_to_list(A), "+", integer_to_list(B), "=", integer_to_list(A+B)], " ").

to_text(ReqData, State) ->
    Key = wrq:path_info(key,ReqData),
    case Key of
        undefined ->
            {"usage: [add] : calculator/add/number1/number2 ", ReqData, State};
        Value ->
            case Value of
                "add" ->
                    Tokens = wrq:path_tokens(ReqData),
                    io:format("calculator command :'add', path_tokens:~p~n", [Tokens]),
                    { add_tokens(Tokens), ReqData, State };
                _ ->
                    {"calculator/add/firstNumber/secondNumber : returns firstNumber + secondNumber", ReqData, State }
            end
    end.

