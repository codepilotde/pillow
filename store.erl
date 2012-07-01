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

-module(store).
-author('Martin Donath <md@struct.cc>').

-export([start/0, loop/2, rec/2, stop/0]).

start() ->

	{ ok, Server } = server:start(?MODULE, 7000, { ?MODULE, loop, [] }),
	Store = dict:new(),
	spawn_link(?MODULE, rec, [ Server, Store ]).

stop() ->
  self() ! { request, stop }.

rec(Server, _) ->
  receive
    % Stop the database server.
    { request, stop } ->
      %server:stop(Server)
      "test"
  end.

% während ausgelesen wird, darf auf keinen fall weiter reingeschrieben werden!
% nach auslesen wieder freigeben!
loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    { ok, Data } ->
      gen_tcp:send(Socket, Data),
      loop(Socket);
    { error, closed } ->
      ok
  end.