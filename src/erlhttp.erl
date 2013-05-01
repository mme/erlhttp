-module(erlhttp).

-version(0.1).
-on_load(init/0).
-export([new/0, parse_raw/2, is_upgrade/1, should_keepalive/1, update/2, parse/1, new_parser_raw/1]).


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
        {done, {Mode1, State1, Rest1, Result1}} -> {done, {Parser, Mode1, State1, Rest1, Result1}};
        {more, {Mode1, State1, Rest1, Result1}} -> {more, {Parser, Mode1, State1, Rest1, Result1}};
        {Other, {Mode1, State1, Rest1, []}, Result1} -> {Other, {Parser, Mode1, State1, Rest1, []}, Result1}
    end.

update({Parser, Mode, State, Rest, Result}, Bin) ->
    case parse_raw(Parser,Bin) of
        {ok, Parsed} -> 
            {ok, {Parser, Mode, State, Rest ++ lists:reverse(Parsed), Result}};
        Error -> 
            Error
    end.
 
%% Request Method
parse(request, _State, [Method={method, _}|Rest], Result) ->
    parse(request,undefined,Rest,[Method|Result]);

%% Request HTTP Version
parse(request, _State, [Version={version, _}|Rest], Result) ->
    parse(request,undefined,Rest,[Version|Result]);

%% Request URL

% - continue
parse(request, {url, Url1}, [{url, Url2}|Rest], Result) ->
    parse(request,{url, <<Url1/binary, Url2/binary>>},Rest,Result);

% - start
parse(request,_,[Url={url, _}|Rest],Result) ->
    parse(request,Url,Rest,Result);
    
% - end URL - return the request and parse headers next
parse(request, Url={url, _}, [Next={header_field, _}|Rest], Result) ->
    {request, {headers,undefined, [Next|Rest], []}, [Url|Result]};

% - end Message
parse(request, Url={url, _}, [done], Result) ->
    {request, {done,undefined, [], []}, [Url|Result]};


% Headers


% | State (prev. callback) | Callback   | Description/action                         |

%  ------------------------ ------------ --------------------------------------------
% | field                  | on_h_field | Previous name continues. Reallocate name   |
% |                        |            | buffer and append callback data to it      |

parse(headers, {header_field, HF1}, [{header_field, HF2}|Rest], Result) ->
    parse(headers, {header_field, <<HF1/binary,HF2/binary>>},Rest, Result);

%  ------------------------ ------------ --------------------------------------------
% | value                  | on_h_value | Value continues. Reallocate value buffer   |
% |                        |            | and append callback data to it             |

parse(headers, {header_field, HF, header_value, HV1}, [{header_value, HV2}|Rest], Result) ->
    parse(headers, {header_field, HF, header_value, <<HV1/binary,HV2/binary>>}, Rest, Result);

%  ------------------------ ------------ --------------------------------------------
% | value                  | on_h_field | New header started.                        |
% |                        |            | Copy current name,value buffers to headers |
% |                        |            | list and allocate new buffer for new name  |

parse(headers, {header_field, HF1, header_value, HV}, [{header_field, HF2}|Rest], Result) ->
    parse(headers, {header_field, HF2},Rest,[{HF1,HV}|Result]);

%  ------------------------ ------------ --------------------------------------------
% | field                  | on_h_value | Value for current header started. Allocate |
% |                        |            | new buffer and copy callback data to it    |

parse(headers, {header_field, HF}, [{header_value, HV}|Rest], Result) ->
    parse(headers, {header_field, HF, header_value, HV}, Rest, Result);

%  ------------------------ ------------ --------------------------------------------
% | nothing (first call)   | on_h_field | Allocate new buffer and copy callback data |
% |                        |            | into it                                    |

parse(headers, undefined, [{header_field, HF}|Rest], Result) ->
    parse(headers, {header_field, HF},Rest,Result);

%  ------------------------ ------------ --------------------------------------------
% | done | body            | on_h_field | Header ended                               |
% |                        |            |                                            |
% |                        |            |                                            |

parse(headers, {header_field, HF1, header_value, HV}, [done], Result) ->
    {headers, {done, undefined, [], []}, [{HF1,HV}|Result]};

parse(headers, {header_field, HF1, header_value, HV}, [Next={body, _}|Rest], Result) ->
    {headers, {body, undefined, [Next|Rest], []}, [{HF1,HV}|Result]};


% Body

parse(body, undefined, [{body, Body}|Rest], Result) ->
    parse(body, undefined, Rest, <<Result/binary,Body/binary>>);

parse(body, undefined, [], Result) ->
    {body, {body, undefined, [], []}, Result};
 
parse(body, undefined, [done], Result) ->
    {body, {done, undefined, [], done}, Result};

% done
parse(done, State, Rest, Result) ->
    {done, {done, State, Rest, Result}, []};

%% need more data
parse(Mode,State,Rest,Result) ->
    {more, {Mode,State, Rest, Result}}.

