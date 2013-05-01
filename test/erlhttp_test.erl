-module(erlhttp_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    {ok, Parser} = erlhttp:new(),
    Request = <<"GET /index.html HTTP/1.1\r\nHost: www.example.com\r\n\r\n">>,
    {ok, Parser1} = erlhttp:update(Parser, Request),
    {request, Parser2, Result} = erlhttp:parse(Parser1),
    ?debugFmt("~p~n", [Result]),
    {headers, _, Result1} = erlhttp:parse(Parser2),
    ?debugFmt("~p~n", [Result1]),
%    Request = <<"POST /hello/kitty HTTP/1.1\r\nHost: www.example.com\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 3\r\n\r\n123\n">>,
    % {ok, Result} = erlhttp:parse_raw(Parser, Request),
    % ?debugFmt("~p~n", [Result]),
    ok.
% 
% post_test() ->
%     {ok, Parser} = erlhttp:new(request),
%     Request = <<"POST /hello/kitty HTTP/1.1\r\nHost: www.example.com\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 3\r\n\r\n123\r\n\r\n">>,
%     Result = erlhttp:parse(Parser, Request),
%     ?debugFmt("~p~n", [Result]),
%     ok.
% 
% parse_test() ->
%     Result = erlhttp:parse(undefined, [{method,post},{url,<<"/hello/kitty">>},{url,<<"/witty">>}], [], [], []),
%     
%     %Result = erlhttp:parse(undefined, [{method,post},{url,<<"/hello/kitty">>},{url,<<"/witty">>}, {header_field, <<"hel">>}, {header_field, <<"lo">>}, {header_value, <<"kit">>}, {header_value, <<"ty">>}, {header_field, <<"2hel">>}, {header_field, <<"lo">>}, {header_value, <<"2kit">>}, {header_value, <<"ty">>}], [], [], []),
%     ?debugFmt("~p~n", [Result]),
%     ok.