% ==========================================================================================================
% MISULTIN - Http(s)
%
% >-|-|-(°>
% 
% Copyright (C) 2010, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
% All rights reserved.
%
% Code portions from Sean Hinde have been originally taken under BSD license from Trapexit at the address:
% <http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features>
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
-module(misultin_http).
-vsn("0.6.0").

% API
-export([handle_data/8]).

% internal
-export([socket_loop/2]).

% macros
-define(MAX_HEADERS_COUNT, 100).
-define(SUPPORTED_ENCODINGS, ["gzip", "deflate"]).

% records
-record(c, {
	sock,
	socket_mode,
	port,
	recv_timeout,
	compress,
	stream_support,
	loop,
	ws_loop
}).

% includes
-include("misultin.hrl").


% ============================ \/ API ======================================================================

% Callback from misultin_socket
handle_data(Sock, SocketMode, ListenPort, PeerAddr, PeerPort, PeerCert, RecvTimeout, CustomOpts) ->
	% build connection & request records
	C = #c{sock = Sock, socket_mode = SocketMode, port = ListenPort, recv_timeout = RecvTimeout, compress = CustomOpts#custom_opts.compress, stream_support = CustomOpts#custom_opts.stream_support, loop = CustomOpts#custom_opts.loop, ws_loop = CustomOpts#custom_opts.ws_loop},
	Req = #req{socket = Sock, socket_mode = SocketMode, peer_addr = PeerAddr, peer_port = PeerPort, peer_cert = PeerCert},
	% enter loop
	request(C, Req).

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% REQUEST: wait for a HTTP Request line. Transition to state headers if one is received. 
request(#c{sock = Sock, socket_mode = SocketMode, recv_timeout = RecvTimeout} = C, Req) ->
	misultin_socket:setopts(Sock, [{active, once}, {packet, http}], SocketMode),
	receive
		{SocketMode, Sock, {http_request, Method, Path, Version}} ->
			?LOG_DEBUG("received full headers of a new HTTP packet", []),
			% change packet type if in ssl mode
			case SocketMode of
				ssl -> misultin_socket:setopts(Sock, [{packet, httph}], SocketMode);
				_ -> ok
			end,
			% go to headers
			headers(C, Req#req{vsn = Version, method = Method, uri = Path, connection = default_connection(Version)}, []);
		{SocketMode, Sock, {http_error, "\r\n"}} ->
			request(C, Req);
		{SocketMode, Sock, {http_error, "\n"}} ->
			request(C, Req);
		{http, Sock, {http_error, _Other}}  ->
			?LOG_WARNING("received a http error, might be a ssl request while socket in http mode: ~p, sending forbidden response and closing socket", [_Other]),
			misultin_socket:send(Sock, misultin_utility:get_http_status_code(403), SocketMode),
			misultin_socket:close(Sock, SocketMode),
			exit(normal);
		_Other ->
			?LOG_WARNING("tcp error on incoming request: ~p, closing socket", [_Other]),
			misultin_socket:close(Sock, SocketMode),
			exit(normal)
	after RecvTimeout ->
		?LOG_DEBUG("normal receive timeout, exit", []),
		misultin_socket:close(Sock, SocketMode),
		exit(normal)
	end.

% HEADERS: collect HTTP headers. After the end of header marker transition to body state.
headers(C, Req, H) ->
	headers(C, Req, H, 0).
headers(#c{sock = Sock, socket_mode = SocketMode}, _Req, _H, ?MAX_HEADERS_COUNT) ->
	?LOG_DEBUG("too many headers sent, bad request",[]),
	misultin_socket:send(Sock, misultin_utility:get_http_status_code(400), SocketMode);
headers(#c{sock = Sock, socket_mode = SocketMode, recv_timeout = RecvTimeout, ws_loop = WsLoop} = C, Req, H, HeaderCount) ->
	misultin_socket:setopts(Sock, [{active, once}], SocketMode),
	receive
		{SocketMode, Sock, {http_header, _, 'Content-Length', _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req#req{content_length = Val}, [{'Content-Length', Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_header, _, 'Connection', _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req#req{connection = keep_alive(Req#req.vsn, Val)}, [{'Connection', Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_header, _, Header, _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, [{Header, Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_error, "\r\n"} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, H, HeaderCount);
		{SocketMode, Sock, {http_error, "\n"} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, H, HeaderCount);
		{SocketMode, Sock, http_eoh} ->
			?LOG_DEBUG("received EOH header", []),
			Headers = lists:reverse(H),
			{_PathType, Path} = Req#req.uri,
			% check if it's a websocket request
			CheckWs = case WsLoop of
				none -> false;
				_Function -> misultin_websocket:check(Path, Headers)
			end,	
			case CheckWs of
				false ->
					?LOG_DEBUG("normal http request received", []),
					body(C, Req#req{headers = Headers});
				{true, Origin, Host, Path} ->
					?LOG_DEBUG("websocket request received", []),
					misultin_websocket:connect(#ws{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, origin = Origin, host = Host, path = Path}, WsLoop)
			end;
		{SocketMode, Sock, _Other} ->
			?LOG_DEBUG("tcp error treating headers: ~p, send bad request error back", [_Other]),
			misultin_socket:send(Sock, misultin_utility:get_http_status_code(400), SocketMode);
		_Other ->
			?LOG_DEBUG("received unknown message: ~p, ignoring", [_Other]),
			ignored
	after RecvTimeout ->
		?LOG_DEBUG("headers timeout, sending request timeout error", []),
		misultin_socket:send(Sock, misultin_utility:get_http_status_code(408), SocketMode)
	end.

% default connection
default_connection({1,1}) -> keep_alive;
default_connection(_) -> close.

% Shall we keep the connection alive? Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
keep_alive({1,1}, "close")		-> close;
keep_alive({1,1}, "Close")		-> close;
% string:to_upper is used only as last resort.
keep_alive({1,1}, Head) ->
	case string:to_upper(Head) of
		"CLOSE" -> close;
		_		-> keep_alive
	end;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive({1,0}, Head) ->
	case string:to_upper(Head) of
		"KEEP-ALIVE"	-> keep_alive;
		_				-> close
	end;
keep_alive({0,9}, _)	-> close;
keep_alive(_Vsn, _KA)	-> close.

% BODY: collect the body of the HTTP request if there is one, and lookup and call the implementation callback.
% Depending on whether the request is persistent transition back to state request to await the next request or exit.
body(#c{sock = Sock, socket_mode = SocketMode, recv_timeout = RecvTimeout} = C, Req) ->
	case Req#req.method of
		'GET' ->
			?LOG_DEBUG("GET request received",[]),
			Close = handle_get(C, Req),
			case Close of
				close ->
					% close socket
					misultin_socket:close(Sock, SocketMode);
				keep_alive ->
					request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, peer_cert = Req#req.peer_cert})
			end;
		'POST' ->
			?LOG_DEBUG("POST request received", []),
			case catch list_to_integer(Req#req.content_length) of
				{'EXIT', _} ->
					% TODO: provide a fallback when content length is not or wrongly specified
					?LOG_DEBUG("specified content length is not a valid integer number: ~p", [Req#req.content_length]),
					misultin_socket:send(Sock, misultin_utility:get_http_status_code(411), SocketMode),
					exit(normal);
				0 ->
					?LOG_DEBUG("zero content-lenght specified, skipping parsing body of request", []),
					Close = handle_post(C, Req),
					case Close of
						close ->
							% close socket
							misultin_socket:close(Sock, SocketMode);
						keep_alive ->
							misultin_socket:setopts(Sock, [{packet, http}], SocketMode),
							request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, peer_cert = Req#req.peer_cert})
					end;					
				Len ->
					?LOG_DEBUG("parsing POST content in body of request", []),
					misultin_socket:setopts(Sock, [{packet, raw}, {active, false}], SocketMode),
					case misultin_socket:recv(Sock, Len, RecvTimeout, SocketMode) of
						{ok, Bin} ->
							Close = handle_post(C, Req#req{body = Bin}),
							case Close of
								close ->
									% close socket
									misultin_socket:close(Sock, SocketMode);
								keep_alive ->
									misultin_socket:setopts(Sock, [{packet, http}], SocketMode),
									request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, peer_cert = Req#req.peer_cert})
							end;
						{error, timeout} ->
							?LOG_DEBUG("request timeout, sending error", []),
							misultin_socket:send(Sock, misultin_utility:get_http_status_code(408), SocketMode); 
						_Other ->
							?LOG_DEBUG("tcp error treating post data: ~p, send bad request error back", [_Other]),
							misultin_socket:send(Sock, misultin_utility:get_http_status_code(400), SocketMode)
					end
			end;
		_Other ->
			?LOG_DEBUG("method not implemented: ~p", [_Other]),
			misultin_socket:send(Sock, misultin_utility:get_http_status_code(501), SocketMode),
			exit(normal)
	end.

% handle a get request
handle_get(C, #req{socket_mode = SocketMode, connection = Conn} = Req) ->
	case Req#req.uri of
		{abs_path, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {abs_path, F}}),
			Conn;
		{absoluteURI, http, _Host, _, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {absoluteURI, F}}),
			Conn;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			misultin_socket:send(C#c.sock, misultin_utility:get_http_status_code(501), SocketMode),
			close;
		{scheme, _Scheme, _RequestString} ->
			misultin_socket:send(C#c.sock, misultin_utility:get_http_status_code(510), SocketMode),
			close;
		_  ->
			misultin_socket:send(C#c.sock, misultin_utility:get_http_status_code(403), SocketMode),
			close
	end.

% handle a post request
handle_post(C, #req{socket_mode = SocketMode, connection = Conn} = Req) ->
	case Req#req.uri of
		{abs_path, _Path} ->
			call_mfa(C, Req),
			Conn;
		{absoluteURI, http, _Host, _, _Path} ->
			call_mfa(C, Req),
			Conn;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			misultin_socket:send(C#c.sock, misultin_utility:get_http_status_code(501), SocketMode),
			close;
		{scheme, _Scheme, _RequestString} ->
			misultin_socket:send(C#c.sock, misultin_utility:get_http_status_code(501), SocketMode),
			close;
		_  ->
			misultin_socket:send(C#c.sock, misultin_utility:get_http_status_code(403), SocketMode),
			close
	end.

% Description: Main dispatcher
call_mfa(#c{sock = Sock, socket_mode = SocketMode, compress = Compress, stream_support = StreamSupport, loop = Loop} = C, #req{headers = RequestHeaders} = Request) ->
	% spawn listening process for Request messages [only used to support stream requests]
	case StreamSupport of
		true ->
			SocketPid = spawn(?MODULE, socket_loop, [C, Request]);
		false ->
			SocketPid = no_stream_support_proc
	end,
	% create request
	Req = misultin_req:new(Request, SocketPid),
	% call loop
	case catch Loop(Req) of
		{'EXIT', _Reason} ->
			?LOG_ERROR("worker crash: ~p", [_Reason]),
			% kill listening socket
			catch SocketPid ! shutdown,
			% send response
			misultin_socket:send(Sock, misultin_utility:get_http_status_code(500), SocketMode),
			% force exit
			exit(normal);
		{HttpCode, Headers0, Body} ->
			% received normal response
			?LOG_DEBUG("sending response", []),
			% kill listening socket
			catch SocketPid ! shutdown,
			% build binary body & compress if necessary
			{CompressHeaders, BodyBinary} = compress_body(RequestHeaders, convert_to_binary(Body), Compress),
			% build headers
			Headers1 = add_output_header('Content-Length', {Headers0, BodyBinary}),
			Headers = add_output_header('Connection', {Headers1, Request}),
			Enc_headers = enc_headers(lists:flatten([CompressHeaders|Headers])),
			% build and send response
			Resp = [misultin_utility:get_http_status_code(HttpCode), Enc_headers, <<"\r\n">>, BodyBinary],
			misultin_socket:send(Sock, Resp, SocketMode);
		{raw, Body} ->
			misultin_socket:send(Sock, Body, SocketMode);
		_ ->
			% loop exited normally, kill listening socket
			catch SocketPid ! shutdown
	end.

% Description: Ensure Body is binary.
convert_to_binary(Body) when is_list(Body) ->
	list_to_binary(lists:flatten(Body));
convert_to_binary(Body) when is_binary(Body) ->
	Body;
convert_to_binary(Body) when is_atom(Body) ->
	list_to_binary(atom_to_list(Body)).

% Description: Socket loop for stream responses
socket_loop(#c{sock = Sock, socket_mode = SocketMode} = C, Request) ->
	receive
		{stream_head, HttpCode, Headers0} ->
			?LOG_DEBUG("sending stream head", []),
			Headers = add_output_header('Connection', {Headers0, Request}),
			Enc_headers = enc_headers(Headers),
			Resp = [misultin_utility:get_http_status_code(HttpCode), Enc_headers, <<"\r\n">>],
			misultin_socket:send(Sock, Resp, SocketMode),
			socket_loop(C, Request);
		{stream_data, Body} ->
			?LOG_DEBUG("sending stream data", []),
			misultin_socket:send(Sock, Body, SocketMode),
			socket_loop(C, Request);
		stream_close ->
			?LOG_DEBUG("closing stream", []),
			misultin_socket:close(Sock, SocketMode);
		shutdown ->
			?LOG_DEBUG("shutting down socket loop", []),
			shutdown
	end.

% Description: Add necessary Content-Length Header
add_output_header('Content-Length', {Headers, Body}) ->
	case misultin_utility:get_key_value('Content-Length', Headers) of
		undefined ->
			[{'Content-Length', size(Body)}|Headers];
		_ExistingContentLength ->
			Headers
	end;

% Description: Add necessary Connection Header
add_output_header('Connection', {Headers, Req}) ->
	case Req#req.connection of
		undefined ->
			% nothing to echo
			Headers;
		Connection ->
			% echo
			case misultin_utility:get_key_value('Connection', Headers) of
				undefined ->
					[{'Connection', connection_str(Connection)}|Headers];
				_ExistingConnectionHeaderValue ->
					Headers
			end
	end.

% Helper to Connection string
connection_str(keep_alive) -> "Keep-Alive";
connection_str(close) -> "Close".

% Description: Encode headers
enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
	[atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
	[Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([]) ->
	[].
enc_header_val(Val) when is_atom(Val) ->
	atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
	integer_to_list(Val);
enc_header_val(Val) ->
	Val.

% Split the path at the ?
split_at_q_mark([$?|T], Acc) ->
	{lists:reverse(Acc), T};
split_at_q_mark([H|T], Acc) ->
	split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
	{lists:reverse(Acc), []}.

% Function: -> {EncodingHeader, binary()}
% Description: Compress body depending on Request Headers and misultin supported encodings.
compress_body(RequestHeaders, BodyBinary, true) ->
	case misultin_utility:get_key_value('Accept-Encoding', RequestHeaders) of
		undefined ->
			% unkown encoding accepted, return body as is
			?LOG_DEBUG("no accepted encoding specified by request: building binary body without compression", []),
			{[], BodyBinary};
		AcceptEncoding ->
			case set_encoding(AcceptEncoding) of
				none ->
					% no common encoding accepted
					?LOG_DEBUG("no supported compression: building binary body without compression", []),
					{[], BodyBinary};
				Encoding ->
					?LOG_DEBUG("building binary body with ~p compression", [Encoding]),
					{{'Content-Encoding', Encoding}, encode(Encoding, BodyBinary)}
			end
	end;
compress_body(_RequestHeaders, BodyBinary, false) ->
	?LOG_DEBUG("building binary body without compression", []),
	{[], BodyBinary}.
			
% Function: -> binary()
% Description: Compress body.
encode(deflate, BodyBinary) ->
	zlib:compress(BodyBinary);
encode(gzip, BodyBinary) ->
	zlib:gzip(BodyBinary).

% Function: -> atom() | none
% Description: Set encoding name depending on Request Headers and supported misultin encodings.
set_encoding(AcceptEncodingHeader) ->
	% get request accepted encodings
	RequestEncodings = get_accepted_encodings(AcceptEncodingHeader),
	% get a request accepted encoding which is supported by misultin
	F = fun({E, _Q}) ->
		lists:member(E, ?SUPPORTED_ENCODINGS)
	end,
	case lists:filter(F, RequestEncodings) of
		[] -> none;
		[{Enc, _Q}|_T] -> list_to_atom(Enc)
	end.

% Function: -> [{Encoding, Q},...]
% Description: Get accepted encodings and quality, sorted by quality.
get_accepted_encodings(AcceptEncodingHeader) ->
	% take away empty spaces
	Header = lists:filter(fun(E) -> case E of $\s -> false; _ -> true end end, AcceptEncodingHeader),
	% get values
	F = fun(E, AccIn) ->
		case string:tokens(E, ";") of
			[Enc] -> [{Enc, 1.0}|AccIn];
			[Enc, QStr] ->
				[_, Val] = string:tokens(QStr, "="),
				case list_to_number(Val) of
					not_a_number -> AccIn;
					V -> [{Enc, V}|AccIn]
				end;
			_ -> AccIn
		end
	end,
	Encodings0 = lists:foldl(F, [], string:tokens(Header, ",")),
	% sort
	lists:sort(fun({_E1, Q1}, {_E2, Q2}) -> Q1 > Q2 end, Encodings0).
			
% Function: -> number() | not_a_number
% Description: Converts a list to a number.		
list_to_number(L) ->
	case catch list_to_float(L) of
		{'EXIT', _} ->
			case catch list_to_integer(L) of
				{'EXIT', _} -> not_a_number;
				Value -> Value
			end;
		Value -> Value
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
