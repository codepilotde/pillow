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

-module(pillow_pusher).
-author("Martin Donath <md@struct.cc>").

% Export functions for public use.
-export([start/2, handle/2]).

% Start a server on the provided port and hand over the Ets instance to the
% callback which is invoked by the server upon an incoming connection. 
start(Port, Ets) ->
  pillow_server:start(Port, { ?MODULE, handle, [Ets] }).

% Handle a TCP connection socket and update the term storage with the data that
% is pushed through the socket.
handle(Socket, [Ets]) ->
  case gen_tcp:recv(Socket, 0) of

    % We received some bytes from the TCP socket, so update the term storage
    % with the key/value combination we found and re-enter the handle loop for
    % further processing.
    { ok, Bytes } ->
      Data = binary_to_list(Bytes) -- [10, 13],
      [Key | Value] = string:tokens(Data, ";"),
      ets:insert(Ets, { Key, Value }),
      handle(Socket, [Ets]);

    % The socket handle was closed, so exit the handle loop.
    { error, closed } ->
      ok
  end.