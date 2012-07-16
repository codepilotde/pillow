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

-module(pillow_export).
-author('Martin Donath <md@struct.cc>').

% Public functions.
-export([start/1, handle/2]).

% Start a server on the provided port and hand over the Ets instance to the
% callback which is invoked by the server upon an incoming connection. 
start(Ets) ->
  { ok, [Port] } = application:get_env(pillow, export),
  pillow_server:start(Port, { ?MODULE, handle, [Ets] }).

% Stream the content of the current term storage (Ets) to the provided socket
% upon request and drop everything afterwards.
handle(Socket, [{ Storage, _ }]) ->
  Data = ets:foldr(fun ({ Key, Value }, List) -> 
    [Key, $\;, Value, $\n | List]                                               % vendor specific code!
  end, [], Storage),
  gen_tcp:send(Socket, Data), gen_tcp:close(Socket),
  ets:delete_all_objects(Storage),
  pillow:stats(gauge, ["pillow.export", 1]),
  ok.