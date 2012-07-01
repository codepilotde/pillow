% Copyright (c) 2012 Martin Donath

% Permission is hereby granted, free of charge, to any person
% obtaining a copy of this software and associated documentation files
% (the "Software"), to deal in the Software without restriction,
% including without limitation the rights to use, copy, modify, merge,
% publish, distribute, sublicense, and/or sell copies of the Software,
% and to permit persons to whom the Software is furnished to do so,
% subject to the following conditions:

% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

-module(pillow_cushion).
-author('Martin Donath <md@struct.cc>').

% Functions exported for public usage.
-export([start/1, logic/1]).

% Start a server on the provided port and hand execution to the event loop.
start(Port) when is_integer(Port) ->
	pillow_server:start(Port, { ?MODULE, logic }).

% If we receive a new package, process data and hand it to the storage.
logic(Socket) ->
  case gen_tcp:recv(Socket, 0) of

    % We received a new data chunk, so process it and hand it to the storage.
    { ok, Data } ->

      %gen_tcp:send(Socket, Data),
      [Key | Value] = string:tokens(Data, " \n"),
      pillow_storage ! { store, Key, Value },
      logic(Socket);

    % An error occurred and the connection was closed.
    { error, closed } ->
      ok
  end.