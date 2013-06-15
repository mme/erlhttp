-module(erlhttp_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

% TODO Implement proper tests

% {ok,[done,
%      {header_value,<<"www.example.com">>},
%      {header_field,<<"Host">>},
%      {version,{1,1}},
%      {url,<<"/index.html">>},
%      {method,get}],
%     <<>>}
    
    
simple_test() ->
    {ok, Parser} = erlhttp:new(),
    Request = <<"GET /index.html HTTP/1.1\r\nHost: www.example.com\r\n\r\n">>,
    {ok, _, Parser1} = erlhttp:update(Request, Parser),
    
    % Result = erlhttp:parse(Parser1),
    % ?debugFmt("~p~n", [Result]),
    
        % 
    {request, Result, Parser2} = erlhttp:parse(Parser1),
    ?debugFmt("~p~n", [Result]),
    % ?debugFmt("~p~n", [Result]),
    {headers, Result1, Parser3} = erlhttp:parse(Parser2),
    ?debugFmt("~p~n", [Result1]),
    ?debugFmt("~p~n", [Parser3]),
    ResultDone = erlhttp:parse(Parser3),
    ?debugFmt("~p~n", [ResultDone]),
    
    % 
%    Request = <<"POST /hello/kitty HTTP/1.1\r\nHost: www.example.com\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 3\r\n\r\n123\n">>,
    % {ok, Result} = erlhttp:parse_raw(Parser, Request),
    % ?debugFmt("~p~n", [Result]),
    ok.

pipeline_test() ->
    {ok, Parser} = erlhttp:new(),
    Request = <<"GET /pipe1 HTTP/1.1\r\n"
    "User-Agent: curl/7.18.0 (i486-pc-linux-gnu) libcurl/7.18.0 OpenSSL/0.9.8g zlib/1.2.3.3 libidn/1.1\r\n"
    "Host: 0.0.0.0=5000\r\n"
    "Accept: /\r\n"
    "Content-Length: 10\r\n"
    "\r\n"
    "0123456789"
    "GET /pipe2 HTTP/1.1\r\n"
    "User-Agent: curl/7.18.0 (i486-pc-linux-gnu) libcurl/7.18.0 OpenSSL/0.9.8g zlib/1.2.3.3 libidn/1.1\r\n"
    "Host: 0.0.0.0=5000\r\n"
    "Content-Length: 10\r\n"
    "\r\n"
    "0123456789">>,
    ?debugFmt("~p~n", [Request]),
    % Request = <<"GET /index.html HTTP/1.1\r\nHost: www.example.com\r\n\r\nGET /index1.html HTTP/1.1\r\nHost: www.example.com\r\n\r\n">>,
    Result = erlhttp:update(Request, Parser),
    ?debugFmt("~p~n", [Result]),
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