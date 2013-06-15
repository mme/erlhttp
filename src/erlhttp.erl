-module(erlhttp).

-version(0.1).
-on_load(init/0).
-export([new/0, parse_raw/2, is_upgrade/1, should_keepalive/1, update/2, parse/1, new_parser_raw/1, clear_body_results/1]).
-include_lib("eunit/include/eunit.hrl").

init() ->
    erlang:load_nif(filename:join(code:priv_dir(?MODULE), atom_to_list(?MODULE) ++ "_nif"), 0).

new_parser_raw(_Type) ->
    exit(nif_library_not_loaded).

parse_raw(_Parser,_Bin) ->
    exit(nif_library_not_loaded).
    
is_upgrade(_Parser) ->
    exit(nif_library_not_loaded).

should_keepalive(_Parser) ->
    exit(nif_library_not_loaded).
    
    
new() ->
    {ok, Parser} = new_parser_raw (0),
    {ok, {Parser, request, undefined, [], []}}.


parse({Parser, Mode, State, Rest, Result}) ->
    case parse(Mode, State, Rest, Result) of
        {more, {Mode1, State1, Rest1, Result1}} -> {more, {Parser, Mode1, State1, Rest1, Result1}};
        {Other, {Mode1, State1, Rest1, []}, Result1} -> {Other, Result1, {Parser, Mode1, State1, Rest1, []} }
    end.

update(Bin,{Parser, Mode, State, Rest, Result}) ->
    case parse_raw(Parser,Bin) of
        {ok, Parsed, Next} -> 
            {ok, Next, {Parser, Mode, State, Rest ++ lists:reverse(Parsed), Result}};
        Error -> 
            Error
    end.

parse(request, State, Rest, Result) ->
    parse_request(State, Rest, Result);

parse(headers, State, Rest, Result) ->
    parse_headers(State, Rest, Result);

parse(body, State, Rest, Result) ->
    parse_body(State, Rest, Result);

parse(done, State, Rest, Result) ->
    parse_done(State, Rest, Result).


parse_request(State, [Next|Rest], Result) ->
    case {State, Next} of
        {undefined, {header_field, _}}      ->  {request, {headers, undefined, [Next|Rest], []}, Result}; 
        {undefined, {body, _}}              ->  {request, {body, undefined, [Next|Rest], <<>>}, Result}; 
        {undefined, done}                   ->  {request, {done, undefined, [Next|Rest], []}, Result}; 
        {undefined, Url={url, _}}           ->  parse_request(Url, Rest, Result); 
        {{url, Url1}, {url, Url2}}          ->  parse_request({url, <<Url1/binary, Url2/binary>>}, Rest, Result); 
        {Url={url, _}, _}                   ->  parse_request(undefined, [Next|Rest], [Url|Result]); 
        {undefined, _}                      ->  parse_request(undefined, Rest, [Next|Result])
    end;

parse_request(State, [], Result) ->
    {more, {request, State, [], Result}, Result}.

 
% Headers

parse_headers(State, [Next|Rest], Result) ->
    
        % | State (prev. callback) | Callback   | Description/action                         |
    case {State, Next} of
    
        %  ------------------------ ------------ --------------------------------------------
        % | nothing (first call)   | on_h_field | Allocate new buffer and copy callback data |
        % |                        |            | into it                                    |
        
        {undefined, {header_field, _}}                                  ->  parse_headers(Next, Rest, Result);
        
        %  ------------------------ ------------ --------------------------------------------
        % | value                  | on_h_field | New header started.                        |
        % |                        |            | Copy current name,value buffers to headers |
        % |                        |            | list and allocate new buffer for new name  |
        
        {{header_field, HF1, header_value, HV}, {header_field, HF2}}    ->  parse_headers({header_field, HF2}, Rest, [{HF1,HV}|Result]);
        
        %  ------------------------ ------------ --------------------------------------------
        % | field                  | on_h_field | Previous name continues. Reallocate name   |
        % |                        |            | buffer and append callback data to it      |
        
        {{header_field, HF1}, {header_field, HF2}}                      ->  parse_headers({header_field, <<HF1/binary,HF2/binary>>}, Rest, Result);
        
        %  ------------------------ ------------ --------------------------------------------
        % | field                  | on_h_value | Value for current header started. Allocate |
        % |                        |            | new buffer and copy callback data to it    |

        {{header_field, HF}, {header_value, HV}}                        ->  parse_headers({header_field, HF, header_value, HV}, Rest, Result);
        
        %  ------------------------ ------------ --------------------------------------------
        % | value                  | on_h_value | Value continues. Reallocate value buffer   |
        % |                        |            | and append callback data to it             |
        
        {{header_field, HF, header_value, HV1}, {header_value, HV2}}    ->  parse_headers({header_field, HF, header_value, <<HV1/binary,HV2/binary>>}, Rest, Result);
        
        
        % start of body
        {{header_field, HF, header_value, HV}, {body, _}}               ->  {headers, {body, undefined, [Next|Rest], <<>>}, [{HF,HV}|Result]}; 
        
        % done
        {{header_field, HF, header_value, HV}, done}                    ->  {headers, {done, undefined, [Next|Rest], []}, [{HF,HV}|Result]}
        
    end;

parse_headers(State, [], Result) ->
    {more, {headers, State, [], Result}, Result}.
    
parse_body(undefined, [{body, Body}|Rest], Result) ->
    parse_body(undefined, Rest, <<Result/binary,Body/binary>>);

parse_body(undefined, [], Result) ->
    {body_chunk, {body, undefined, [], Result}, Result};

parse_body(undefined, [done], Result) ->
    {body_chunk, {done, undefined, [done], []}, Result}.

parse_done(undefined, [done], _Result) ->
    {done, {done, undefined, [done], []}, done}.
    
clear_body_results({Parser, body, undefined, Rest, _Result}) ->
    {Parser, body, undefined, Rest, <<>>}.

