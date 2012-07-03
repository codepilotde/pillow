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
-author('Martin Donath <md@struct.cc>').

% Public functions.
-export([start/2, handle/2, write/2]).

% Start a server on the provided port and hand over the Ets instance to the
% callback which is invoked by the server upon an incoming connection. 
start(Port, Ets) ->
  pillow_server:start(Port, { ?MODULE, handle, [Ets] }).

% Handle a TCP connection socket and update the term storage with the data that
% is pushed through the socket.
handle(Socket, [Ets]) ->
  case gen_tcp:recv(Socket, 0) of

    % We received some bytes from the TCP socket, so update the term storage
    % after splitting the data we found into a key/value combination and re-
    % enter the handle loop for further processing.
    { ok, Bytes } ->
      spawn(?MODULE, write, [binary_to_list(Bytes), Ets]),
      handle(Socket, [Ets]);

    % The socket handle was closed, so exit the event loop.
    { error, closed } ->
      ok
  end.

% Split the provided data at line breaks and write each entry into the term
% storage. If the second element is an empty list, we're done.
write(Data, Ets) ->
  { Entry, Rest } = split(Data, $\n),
  ets:insert(Ets, split(Entry, $\;)),
  case Rest of
    [] -> ok;
    __ -> write(Rest, Ets)
  end.

% Split the string/list at the first occurence of the provided separator and
% return both lists in a tuple.
split(List, Separator) ->
  split(List, [], Separator).
split(List, Memory, Separator) when is_list(List) and is_integer(Separator) ->
  case List of
    [Separator | Tail] ->
      { [], Tail };
    [Head | Tail] ->
      { L, M } = split(Tail, Memory, Separator), { [Head | L], M };
    [] ->
      { List, Memory }
  end.