% Copyright (c) 2012 Martin Donath <md@struct.cc>

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

-module(pillow_inflow).
-author('Martin Donath <md@struct.cc>').

% Public functions.
-export([start/1, handle/2, stats/1]).

% Start a server on the provided port and hand over a callback which is
% executed upon a successful connection. This callback must return another
% valid callback which is then invoked upon receiving a new line of data.
start(Ets) ->
  { ok, [Port, Callback] } = application:get_env(pillow, inflow),
  timer:apply_interval(10000, ?MODULE, stats, [Ets]),
  pillow_server:start(Port, { ?MODULE, handle, [Ets, Callback] }).

% Handle the data streamed over the socket and push it into the term storage.
% Also, notify any subscribed clients on specific updates.
handle(Socket, [Ets, Callback]) ->
  pillow:stats(increment, ["pillow.inflow.clients"]),
  handle(Socket, <<>>, [Ets, Callback]).
handle(Socket, Prefix, [Ets, { Module, Function }]) ->
  case gen_tcp:recv(Socket, 0) of

    % We received some bytes from the socket, so spawn a new process to process
    % the data and re-enter the event loop to receive further data.
    { ok, Bytes } ->
      { Line, Rest } = partition(<<Prefix/bitstring, Bytes/bitstring>>),
      spawn_link(Module, Function, [Line, Ets]),
      handle(Socket, Rest, [Ets, { Module, Function }]);

    % The socket handle was closed, so update stats and exit the event loop.
    { error, closed } ->
      pillow:stats(decrement, ["pillow.inflow.clients"]),
      ok
  end.

% -----------------------------------------------------------------------------
% Private
% -----------------------------------------------------------------------------

% Split a set of bytes at the last linefeed and return the two Bitstrings.
partition(Bytes) ->
  Size = bit_size(Bytes), <<Integer:Size/integer>> = Bytes,
  partition(Bytes, Integer, 0).
partition(Bytes, Integer, Offset) ->
  case (Integer band (255 bsl Offset * 8)) bsr Offset * 8 of
    10 ->
      Cuts = bit_size(Bytes) - Offset * 8,
      <<Left:Cuts/bits, Right/bits>> = Bytes,
      { Left, Right };
    0 ->
      { <<>>, Bytes };
    _ ->
      partition(Bytes, Integer, Offset + 1)
  end.

% Report regular statistics.
stats({ Storage, _ }) ->
  pillow:stats(gauge, ["pillow.inflow.unique", ets:info(Storage, size)]).