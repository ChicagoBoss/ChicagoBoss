% ==========================================================================================================
% MISULTIN - WebSocket
%
% >-|-|-(°>
% 
% Copyright (C) 2010, Roberto Ostinelli <roberto@ostinelli.net>, Joe Armstrong.
% All rights reserved.
%
% Code portions from Joe Armstrong have been originally taken under MIT license at the address:
% <http://armstrongonsoftware.blogspot.com/2009/12/comet-is-dead-long-live-websockets.html>
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(misultin_websocket).
-vsn("0.6.0").

% API
-export([check/2, connect/2]).


% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {true, Origin, Host, Path} | false
% Description: Check if the incoming request is a websocket handshake.
check(Path, Headers) ->
	?LOG_DEBUG("testing for a websocket request, path: ~p headers: ~p", [Path, Headers]),
	case Headers of
		[{'Upgrade', "WebSocket"}, {'Connection', "Upgrade"}, {'Host', Host}, {"Origin", Origin}|_RH] ->
			% websockets request
			{true, Origin, Host, Path};
		_Else ->
			% normal HTTP request
			false
	end.

% Connect and handshake with Websocket.
connect(#ws{socket = Socket, socket_mode = SocketMode, origin = Origin, host = Host, path = Path} = Ws, WsLoop) ->
	?LOG_DEBUG("received websocket handshake request", []),
	HandshakeServer = ["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"WebSocket-Origin: ", Origin , "\r\n",
		"WebSocket-Location: ws://", lists:concat([Host, Path]), "\r\n\r\n"
	],
	% send handshake back
	misultin_socket:send(Socket, HandshakeServer, SocketMode),
	% spawn controlling process
	Ws0 = misultin_ws:new(Ws, self()),
	WsHandleLoopPid = spawn(fun() -> WsLoop(Ws0) end),
	erlang:monitor(process, WsHandleLoopPid),
	% set opts
	misultin_socket:setopts(Socket, [{packet, 0}, {active, true}], SocketMode),
	% add main websocket pid to misultin server reference
	misultin:persistent_socket_pid_add(self()),
	% start listening for incoming data
	ws_loop(Socket, none, WsHandleLoopPid, SocketMode).	
	
% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Main Websocket loop
ws_loop(Socket, Buffer, WsHandleLoopPid, SocketMode) ->
	receive
		{tcp, Socket, Data} ->
			handle_data(Buffer, binary_to_list(Data), Socket, WsHandleLoopPid, SocketMode);
		{tcp_closed, Socket} ->
			?LOG_DEBUG("tcp connection was closed, exit", []),
			% close websocket and custom controlling loop
			websocket_close(Socket, WsHandleLoopPid, SocketMode);
		{'DOWN', Ref, process, WsHandleLoopPid, Reason} ->
			case Reason of
				normal ->
					?LOG_DEBUG("linked websocket controlling loop stopped.", []);
				_ ->
					?LOG_ERROR("linked websocket controlling loop crashed with reason: ~p", [Reason])
			end,
			% demonitor
			erlang:demonitor(Ref),
			% close websocket and custom controlling loop
			websocket_close(Socket, WsHandleLoopPid, SocketMode);
		{send, Data} ->
			?LOG_DEBUG("sending data to websocket: ~p", [Data]),
			misultin_socket:send(Socket, [0, Data, 255], SocketMode),
			ws_loop(Socket, Buffer, WsHandleLoopPid, SocketMode);
		shutdown ->
			?LOG_DEBUG("shutdown request received, closing websocket with pid ~p", [self()]),
			% close websocket and custom controlling loop
			websocket_close(Socket, WsHandleLoopPid, SocketMode);
		_Ignored ->
			?LOG_WARNING("received unexpected message, ignoring: ~p", [_Ignored]),
			ws_loop(Socket, Buffer, WsHandleLoopPid, SocketMode)
	end.

% Buffering and data handling
handle_data(none, [0|T], Socket, WsHandleLoopPid, SocketMode) ->
	handle_data([], T, Socket, WsHandleLoopPid, SocketMode);
handle_data(none, [], Socket, WsHandleLoopPid, SocketMode) ->
	ws_loop(Socket, none, WsHandleLoopPid, SocketMode);
handle_data(L, [255|T], Socket, WsHandleLoopPid, SocketMode) ->
	WsHandleLoopPid ! {browser, lists:reverse(L)},
	handle_data(none, T, Socket, WsHandleLoopPid, SocketMode);
handle_data(L, [H|T], Socket, WsHandleLoopPid, SocketMode) ->
	handle_data([H|L], T, Socket, WsHandleLoopPid, SocketMode);
handle_data([], L, Socket, WsHandleLoopPid, SocketMode) ->
	ws_loop(Socket, L, WsHandleLoopPid, SocketMode).

% Close socket and custom handling loop dependency
websocket_close(Socket, WsHandleLoopPid, SocketMode) ->
	% remove main websocket pid from misultin server reference
	misultin:persistent_socket_pid_remove(self()),
	% kill custom handling loop process
	exit(WsHandleLoopPid, kill),
	% close main socket
	misultin_socket:close(Socket, SocketMode).

% ============================ /\ INTERNAL FUNCTIONS =======================================================
