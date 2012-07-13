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
-export([start/3, handle/2, process/3, stream/2, statistics/1]).

% Start a server on the provided port and hand over the Ets instances to the
% callback which is invoked by the server upon an incoming connection. 
start(Port, Storage, Clients) ->
  timer:apply_interval(10000, ?MODULE, statistics, [Storage]),
  Pid = pillow_server:start(Port, { ?MODULE, handle, [Storage, Clients] }),
  estatsd:gauge("pillow.inflow.boot", 1), Pid.

% Handle the data streamed over the socket and push it into the term storage.
% Also, notify any subscribed clients on specific updates.
handle(Socket, [Storage, Clients]) ->
  estatsd:increment("pillow.inflow.clients"),
  handle(Socket, <<>>, [Storage, Clients]).
handle(Socket, Rest, [Storage, Clients]) ->
  case gen_tcp:recv(Socket, 0) of

    % We received some bytes from the socket, so spawn a new process to process
    % the data and re-enter the event loop to receive further data.
    { ok, Bytes } ->
      { Left, Right } = partition(<<Rest/bitstring, Bytes/bitstring>>),
      spawn_link(?MODULE, process, [Left, Storage, Clients]),
      handle(Socket, Right, [Storage, Clients]);

    % The socket handle was closed, so exit the event loop.
    { error, closed } ->
      estatsd:decrement("pillow.inflow.clients"),
      ok
  end.

% --- CUSTOM FUNCTIONALITY --> EXTRA MODULE ! REPOSITORY ----------------------

% Split the provided data at line breaks into key/value combinations and              % COMMENTS
% process each entry by writing/streaming it.
process(Bytes, Storage, Clients) ->
  Data = binary_to_list(Bytes),
  case Data of
    [1 | Timestamp] ->
      % --- BETA --------------------------------------------------------------
      [Y, M, D, H, I, S] = lists:map(fun (Value) ->
        list_to_integer(Value)
      end, string:tokens(Timestamp -- [13, 10], ". :;")),
      % --- BETA --------------------------------------------------------------
      T = calendar:datetime_to_gregorian_seconds({ { Y, M, D }, { H, I, S } }),
      L = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
      estatsd:timing("pillow.inflow.delay", (L - T + 1));
      % --- BETA --------------------------------------------------------------
    _ ->
      Updates = process(Data, Storage, Clients, 0),
      estatsd:increment("pillow.inflow.total", Updates)
  end,
  ok.
process([], _, _, Updates) ->
  Updates;
process(Data, Storage, Clients, Updates) ->
  { Entry, Rest } = separate(Data, $\n),
  ets:insert(Storage, { Key, _ } = separate(Entry, $\;)),

  % Check, if one or more clients subscribed for updates on the current key.
  % If so, stream the current entry to all of them.
  case ets:lookup(Clients, Key) of
    [{ _, Pids }] ->
      stream(Entry, Pids);
    _ ->
      ok
  end,

  % If we still have data to process, call the process function recursively.
  case Rest of
    [] ->
      Updates;
    _ ->
      process(Rest, Storage, Clients, Updates + 1)
  end.

% Stream an entry to the list of provided processes.
stream(_, []) ->
  ok;
stream(Entry, [Pid | Rest]) ->
  Pid ! { update, Entry },
  stream(Entry, Rest).

% -----------------------------------------------------------------------------
% Private
% -----------------------------------------------------------------------------

% Partition a set of bytes at the last linefeed into two bitstrings. The second
% bitstring is then appended in front of the string read from the socket.
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

% Split the string/list at the first occurence of the provided separator and
% return both lists in a tuple.
separate(List, Separator) ->
  separate(List, [], Separator).
separate(List, Memory, Separator) when is_list(List) and is_integer(Separator) ->
  case List of
    [Separator | Tail] ->
      { [], Tail };
    [Head | Tail] ->
      { L, M } = separate(Tail, Memory, Separator), { [Head | L], M };
    [] ->
      { List, Memory }
  end.

% Send some statistics to estatsd.
statistics(Storage) ->
  estatsd:gauge("pillow.inflow.unique", ets:info(Storage, size)).