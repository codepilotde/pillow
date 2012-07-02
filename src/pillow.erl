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
-author("Martin Donath <md@struct.cc>").

% Export functions demanded by the OTP Application behaviour.
-behaviour(application).
-export([start/2, stop/1, init/1]).

% Export functions demanded by the OTP Generic Server behaviour.
-export([setup/1, loop/1, push/1, dump/1, bbsl/2, bbsr/2, brol/2]).

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

% Spawn the event loop and start two servers for pushing data into and dumping
% data from the dictionary.
setup([Push, Dump]) ->
  register(?MODULE, spawn_link(?MODULE, loop, [
    ets:new(data, [ordered_set, protected
  ])])),
  pillow_server:start(Push, { ?MODULE, push }),
  pillow_server:start(Dump, { ?MODULE, dump }),
  { ok, self() }.

% TBD
loop(Ets) ->
  receive

    % We received a set of bytes, so shift the received data right by 8 bytes
    % in order to truncate the line feed and store the data.
    { store, Bytes } ->
      Size = bit_size(Bytes) - 8, <<Rest:Size/bits, _:8>> = Bytes,
      Data = binary_to_list(<<0:8, Rest/bits>>),
      loop(Ets)
  end.


push(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    { ok, Bytes } ->
      ?MODULE ! { store, Bytes },
      push(Socket);
    { error, closed } ->
      ok
  end.

dump(Socket) ->
  dump(Socket).
  
  
bbsr(Bin, Shift) ->
  Size = bit_size(Bin) - Shift,
  <<Rest:Size/bits, _:Shift>> = Bin, <<0:Shift, Rest/bits>>.
  
bbsl(Bin, Shift) ->
  <<_:Shift, Rest/bits>> = Bin, <<Rest/bits, 0:Shift>>.

brol(Bin,Shift) ->
  <<U:Shift,Rest/bits>> = Bin,
  <<Rest/bits,U:Shift>>.