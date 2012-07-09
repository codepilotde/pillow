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

-module(pillow_stream).
-author('Martin Donath <md@struct.cc>').

% Public functions.
-export([start/2, handle/2, update/1]).

% Start a server on the provided port and hand over the Ets instance to the
% callback which is invoked by the server upon an incoming connection.
start(Port, Clients) ->
  pillow_server:start(Port, { ?MODULE, handle, [Clients] }).

% Subscribe to the keys transferred over the socket.
handle(Socket, [Clients]) ->
  handle(Socket, spawn_link(?MODULE, update, [Socket]), [Clients]).
handle(Socket, Pid, [Clients]) ->
  case gen_tcp:recv(Socket, 0) of

    % We received a list of keys to subscribe to, so split all bytes at
    % linefeeds to obtain the keys and wait for further keys.
    { ok, Bytes } ->
      Keys = string:tokens(binary_to_list(Bytes) -- [13, 10], [$\;, $\n]),
      subscribe(Keys, Pid, Clients),
      handle(Socket, Pid, [Clients]);

    % The socket handle was closed, so unregister the process from all keys and
    % exit the event loop.
    { error, closed } ->
      unsubscribe(Pid, Clients),
      ok
  end.

% Receive updates and send them over the socket.
update(Socket) ->
  receive
    { update, Entry } ->
      gen_tcp:send(Socket, Entry ++ "\n"),
      update(Socket);
    _ ->
      ok
  end.

% -----------------------------------------------------------------------------
% Private
% -----------------------------------------------------------------------------

% Subscribe the update process to a set of keys.
subscribe([], _, _) ->
  ok;
subscribe([Key | Rest], Pid, Clients) ->
  case ets:lookup(Clients, Key) of
    [] ->
      ets:insert(Clients, { Key, [Pid] });
    [{ _, Pids }] ->
      ets:insert(Clients, { Key, [Pid | Pids] })
  end,
  subscribe(Rest, Pid, Clients).

% Unsubscribe the update process from all keys.
unsubscribe(Pid, Clients) ->
  ets:foldl(fun({ Key, Value }, _) -> 
    case Value -- [Pid] of
      [] ->
        ets:delete(Clients, Key);
      Pids ->
        ets:insert(Clients, { Key, Pids })
    end
  end, [], Clients),
  ok.