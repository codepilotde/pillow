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

-module(pillow_server).
-author('Martin Donath <md@struct.cc>').

% Functions demanded by the OTP Generic Server behaviour.
-behaviour(gen_server).
-export([init/1, terminate/2, handle_cast/2,
         handle_info/2, handle_call/3, code_change/3]).

% Internal functions.
-export([accept_loop/1]).

% Public functions.
-export([start/2]).

% The server state contains the port the server should be listening on, the
% socket, as well as a tuple containing a reference to the callback function.
-record(state, { port, call, ip = any, socket = null }).
-define(OPTIONS, [ binary, { packet, 0 }, { active, false }, { reuseaddr, true }]).

% Start a server on the provided port and register the provided callback.
% This method is used to start the server module from outside.
start(Port, Callback) when is_integer(Port), is_tuple(Callback) ->
  gen_server:start_link(?MODULE, #state{ port = Port, call = Callback }, []).

% Initialize a TCP server on the provided port and handle incoming
% connections, called by gen_server:start_link/3.
init(State = #state{ port = Port }) ->
  case gen_tcp:listen(Port, ?OPTIONS) of

    % The listening socket could be instantiated, so call the accept function..
    { ok, Listen } ->
      { ok, accept(State#state{ socket = Listen }) };

    % Listening on the socket failed for some reason.
    { error, Reason } ->
      { stop, Reason }
  end.

% An incoming connection was accepted on the listening socket, so spawn a new
% process invoking the accept loop.
accept(State = #state{ socket = Listen, call = Callback }) ->
  spawn_link(?MODULE, accept_loop, [{ self(), Listen, Callback }]),
  State.

% When an incoming connection occurs, call the provided callback.
accept_loop({ Server, Listen, { Module, Function, Params } }) ->
  case gen_tcp:accept(Listen) of

    % A connection could be established, so execute our callback.
    { ok, Socket } ->
      gen_server:cast(Server, { accepted, self() }),
      Module:Function(Socket, Params);

    % The connection failed for some reason or was closed.
    { error, _ } ->
      ok
  end.

% Callback for asynchronous server calls, spawn a new acceptor.
handle_cast({ accepted, _Pid }, State = #state{} ) ->
  { noreply, accept(State) }.

% Callback for synchronous server calls.
handle_call(Request, _From, State) ->
  { stop, { unknown_call, Request }, State }.

% Don't handle any messages directly sent to the server's mailbox.
handle_info(_Info, State) ->
  { noreply, State }.

% Callback executed on server shutdown.
terminate(_Reason, State) ->
  gen_tcp:close(State#state.socket),
  ok.

% Convert the process state when the code of the server is changed.
code_change(_Old, State, _Extra) ->
  { ok, State }.