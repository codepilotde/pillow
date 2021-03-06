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

-module(pillow).
-author('Martin Donath <md@struct.cc>').

% Export functions demanded by the OTP Application behaviour.
-behaviour(application).
-export([start/2, stop/1, init/1, stats/2]).

% Define parameters for supervisor specification.
-define(MAX_RESTART,  5).
-define(MAX_SECONDS, 60).

% Start the pillow application by initializing the supervisor.
start(_Type, _Args) ->
  Pid = supervisor:start_link(?MODULE, []),
  pillow:stats(gauge, ["pillow.boot", 1]), Pid.

% Executed after the application is stopped.
stop(_State) ->
  ok.

% Setup term storages and return the worker specifications for the supervisor.
init(_) ->
  Ets = {
    ets:new(pillow_storage, [set, public]),
    ets:new(pillow_clients, [set, public])
  },
  { ok, {
    { one_for_one, ?MAX_RESTART, ?MAX_SECONDS }, [
      { pillow_export, { pillow_export, start, [Ets] },
        permanent, 2000, worker, []
      },
      { pillow_inflow, { pillow_inflow, start, [Ets] },
        permanent, 2000, worker, []
      },
      { pillow_stream, { pillow_stream, start, [Ets] },
        permanent, 2000, worker, []
      }
    ]
  } }.

% Send statistics to estatsd.
stats(Function, Args) ->
  erlang:apply(estatsd, Function, Args).