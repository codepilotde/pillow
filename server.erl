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

-module(server).
-author('Martin Donath <md@struct.cc>').

% Export functions demanded by the OTP Generic Server behaviour.
-behaviour(gen_server).
-export([init/1, terminate/2, handle_info/2,
         handle_cast/2, handle_call/3, code_change/3]).

% Internal function for asynchronous execution of the provided callback.
-export([execute/1]).

% These are the functions that are exported to communicate with the server.
-export([start/3]).

% The server state contains the port the server should be listening on, as well
% as a tuple containing a reference to the callback function (loop).
-record(state, { port, call, ip = any, socket = null }).
-define(OPTIONS, [ binary, { packet, 0 }, { active, false }, { reuseaddr, true }]).

% Start a server on the provided port and register the provided callback.
% This method is used to start the server module from outside.
start(Name, Port, Callback) when is_integer(Port), is_tuple(Callback) ->
  gen_server:start_link(
    { local, Name }, ?MODULE, #state{ port = Port, call = Callback }, []
  ).

% Initialize a TCP server on the provided port and handle incoming
% connecitons, called by gen_server:start_link/4.
init(State = #state{ port = Port }) ->
  case gen_tcp:listen(Port, ?OPTIONS) of

    % The listening socket could be instantiated, so call the accept function
    % after adding a reference to the listening socket.
    { ok, Listen } ->
      { ok, accept(State#state{ socket = Listen }) };

    % The connection failed for some reason.
    { error, Reason } ->
     	{ stop, Reason }
  end.

% Accept an incoming connection on the listening socket.
accept(State = #state{ socket = Listen, call = Callback }) ->
	spawn_link(?MODULE, execute, [{ self(), Listen, Callback }]),
	State.

% Execute the provided callback asynchronously.
execute({ Server, Listen, { Module, Function } }) ->
	{ ok, Socket } = gen_tcp:accept(Listen),
	gen_server:cast(Server, { accepted, self() }),
	Module:Function(Socket).

% --- Just implemented for convenience. TBD: Robustness!
handle_cast({ accepted, _Pid }, State = #state{} ) -> { noreply, State }.
handle_call(_Request, _From, State) -> { noreply, State }.
handle_info(_Request, Library) -> { noreply, Library }.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> { ok, Library }.