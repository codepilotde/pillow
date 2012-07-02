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

-module(pillow).
-author('Martin Donath <md@struct.cc>').

% Export functions demanded by the OTP Application behaviour.
-behaviour(application).
-export([start/2, stop/1, init/1]).

% Export functions demanded by the OTP Generic Server behaviour.
-export([setup/1, loop/1, push/1, dump/1]).

% Define parameters for supervisor specification.
-define(MAX_RESTART,  1).
-define(MAX_SECONDS, 60).

% Start the pillow application by initializing the supervisor.
start(_Type, Args) ->
  supervisor:start_link(?MODULE, Args).

% Executed after the application is stopped.
stop(_State) ->
  ok.

% Setup and return the specification for the supervisor.
init(Ports) ->
  { ok, {
    { one_for_one, ?MAX_RESTART, ?MAX_SECONDS }, [
      { undefined, { ?MODULE, setup, [Ports] },
        temporary, 2000, worker, []
      }
    ]
  } }.

% Start two servers for pushing data into and dumping data from the dictionary.
setup([Push, Dump]) ->
	spawn_link(?MODULE, loop, [dict:new()]), % TODO. use ETS instead of DICT.
	pillow_server:start(Push, { ?MODULE, push }),
	pillow_server:start(Dump, { ?MODULE, dump }),
	{ ok, self() }.

% TBD
loop(Dict) ->
  receive
    { ok, Data } ->
      loop(Dict)
  end.

push(Socket) ->
  push(Socket).

dump(Socket) ->
  case gen_tcp:recv(Socket, 0) of

    % We received a new data chunk, so process it and hand it to the storage.
    { ok, Data } ->
      gen_tcp:send(Socket, Data),
      dump(Socket);

    % An error occurred and the connection was closed.
    { error, closed } ->
      ok
  end.