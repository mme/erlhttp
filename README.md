erlhttp
================================

*erlang NIFs for http-parser*, see https://github.com/joyent/http-parser


Usage
-------------------------

```erlang
{ok, Parser} = erlhttp:new(),
Request = <<"GET /index.html HTTP/1.1\r\nHost: www.example.com\r\n\r\n">>,
{ok, Rest, Parser1} = erlhttp:update(Request, Parser), 
% In a pipelined connection, Rest will contain parts of the next request 
{request, Result, Parser2} = erlhttp:parse(Parser1),
% Result is now [{url,<<"/index.html">>},{version,{1,1}},{method,get}]
erlhttp:parse(Parser2),
% continue parsing with the state returned from the previous call to parse
```

```erlhttp:parse``` return values:

```{request, Result, Parser}``` finished parsing request method, url & http version

```{headers, Result, Parser}``` finished parsing headers

```{body, Result, Parser}``` finished parsing a body chunk

```{done, Result, Parser}``` done parsing

```{more, Result, Parser}``` need more data, call erlhttp:parse/2

The parser will automatically accumulate the request body. To clear the results of the request body, e.g. after writing to a file, call ```clear_body_results(Parser)```

License
-------------------------
Copyright (c) 2013, Markus Ecker

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



