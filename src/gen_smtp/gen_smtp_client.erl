%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc A simple SMTP client used for sending mail - assumes relaying via a
%% smarthost.

-module(gen_smtp_client).

-define(DEFAULT_OPTIONS, [
		{ssl, false}, % whether to connect on 465 in ssl mode
		{tls, if_available}, % always, never, if_available
		{auth, if_available},
		{hostname, smtp_util:guess_FQDN()},
		{retries, 1} % how many retries per smtp host on temporary failure
	]).

-define(AUTH_PREFERENCE, [
		"CRAM-MD5",
		"LOGIN",
		"PLAIN"
	]).

-define(TIMEOUT, 1200000).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-else.
-export([send/2, send_blocking/2, send_it/2]).
-endif.

-spec send(Email :: {string(), [string(), ...], string()}, Options :: list()) -> {'ok', pid()} | {'error', any()}.
send(Email, Options) ->
	NewOptions = lists:ukeymerge(1, lists:sort(Options),
		lists:sort(?DEFAULT_OPTIONS)),
	case check_options(NewOptions) of
		ok ->
			Pid = spawn_link(fun () ->
						send_it_nonblock(Email, NewOptions)
				end
			),
			{ok, Pid};
		{error, Reason} ->
			{error, Reason}
	end.

-spec send_blocking(Email :: {string() | binary(), [string() | binary(), ...], string() | binary()}, Options :: list()) -> binary() | {'error', atom(), any()}.
send_blocking(Email, Options) ->
	NewOptions = lists:ukeymerge(1, lists:sort(Options),
		lists:sort(?DEFAULT_OPTIONS)),
	case check_options(NewOptions) of
		ok ->
			send_it(Email, NewOptions);
		{error, Reason} ->
			{error, Reason}
	end.

send_it_nonblock(Email, Options) ->
	case (?MODULE):send_it(Email, Options) of
		{error, Type, Message} ->
			erlang:exit({error, Type, Message});
		_ ->
			ok
	end.

-spec send_it(Email :: {string(), [string(), ...], string()}, Options :: list()) -> 'ok'.
send_it(Email, Options) ->
	RelayDomain = proplists:get_value(relay, Options),
	MXRecords = case proplists:get_value(no_mx_lookups, Options) of
		true ->
			[];
		_ ->
			smtp_util:mxlookup(RelayDomain)
	end,
	%io:format("MX records for ~s are ~p~n", [RelayDomain, MXRecords]),
	Hosts = case MXRecords of
		[] ->
			[{0, RelayDomain}]; % maybe we're supposed to relay to a host directly
		_ ->
			MXRecords
	end,
	try_smtp_sessions(Hosts, Email, Options, []).

try_smtp_sessions([{Distance, Host} | Tail], Email, Options, RetryList) ->
	Retries = proplists:get_value(retries, Options),
	try do_smtp_session(Host, Email, Options)
	catch
		throw:{permanant_failure, Message} ->
			% permanant failure means no retries, and don't even continue with other hosts
			{error, no_more_hosts, {permanant_failure, Host, Message}};
		throw:{FailureType, Message} ->
			case proplists:get_value(Host, RetryList) of
				RetryCount when is_integer(RetryCount), RetryCount >= Retries ->
					% out of chances
					%io:format("retries for ~s exceeded (~p of ~p)~n", [Host, RetryCount, Retries]),
					NewHosts = Tail,
					NewRetryList = lists:keydelete(Host, 1, RetryList);
				RetryCount when is_integer(RetryCount) ->
					%io:format("scheduling ~s for retry (~p of ~p)~n", [Host, RetryCount, Retries]),
					NewHosts = Tail ++ [{Distance, Host}],
					NewRetryList = lists:keydelete(Host, 1, RetryList) ++ [{Host, RetryCount + 1}];
				_ when Retries == 0 ->
					% done retrying completely
					NewHosts = Tail,
					NewRetryList = lists:keydelete(Host, 1, RetryList);
				_ ->
					% otherwise...
					%io:format("scheduling ~s for retry (~p of ~p)~n", [Host, 1, Retries]),
					NewHosts = Tail ++ [{Distance, Host}],
					NewRetryList = lists:keydelete(Host, 1, RetryList) ++ [{Host, 1}]
			end,
			case NewHosts of
				[] ->
					{error, retries_exceeded, {FailureType, Host, Message}};
				_ ->
					try_smtp_sessions(NewHosts, Email, Options, NewRetryList)
			end
	end.

do_smtp_session(Host, Email, Options) ->
	{ok, Socket, _Host, _Banner} = connect(Host, Options),
	%io:format("connected to ~s; banner was ~s~n", [Host, Banner]),
	{ok, Extensions} = try_EHLO(Socket, Options),
	%io:format("Extensions are ~p~n", [Extensions]),
	{Socket2, Extensions2} = try_STARTTLS(Socket, Options,
		Extensions),
	%io:format("Extensions are ~p~n", [Extensions2]),
	_Authed = try_AUTH(Socket2, Options, proplists:get_value(<<"AUTH">>, Extensions2)),
	%io:format("Authentication status is ~p~n", [Authed]),
	Receipt = try_sending_it(Email, Socket2, Extensions2),
	%io:format("Mail sending successful~n"),
	quit(Socket2),
	Receipt.

try_sending_it({From, To, Body}, Socket, Extensions) ->
	try_MAIL_FROM(From, Socket, Extensions),
	try_RCPT_TO(To, Socket, Extensions),
	try_DATA(Body, Socket, Extensions).

try_MAIL_FROM(From, Socket, Extensions) when is_binary(From) ->
	try_MAIL_FROM(binary_to_list(From), Socket, Extensions);
try_MAIL_FROM("<" ++ _ = From, Socket, _Extensions) ->
	% TODO do we need to bother with SIZE?
	socket:send(Socket, ["MAIL FROM: ", From, "\r\n"]),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"250", _Rest/binary>>} ->
			true;
		{ok, <<"4", _Rest/binary>> = Msg} ->
			quit(Socket),
			throw({temporary_failure, Msg});
		{ok, Msg} ->
			%io:format("Mail FROM rejected: ~p~n", [Msg]),
			quit(Socket),
			throw({permanant_failure, Msg})
	end;
try_MAIL_FROM(From, Socket, Extensions) ->
	% someone was bad and didn't put in the angle brackets
	try_MAIL_FROM("<"++From++">", Socket, Extensions).

try_RCPT_TO([], _Socket, _Extensions) ->
	true;
try_RCPT_TO([To | Tail], Socket, Extensions) when is_binary(To) ->
	try_RCPT_TO([binary_to_list(To) | Tail], Socket, Extensions);
try_RCPT_TO(["<" ++ _ = To | Tail], Socket, Extensions) ->
	socket:send(Socket, ["RCPT TO: ",To,"\r\n"]),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"250", _Rest/binary>>} ->
			try_RCPT_TO(Tail, Socket, Extensions);
		{ok, <<"251", _Rest/binary>>} ->
			try_RCPT_TO(Tail, Socket, Extensions);
		{ok, <<"4", _Rest/binary>> = Msg} ->
			quit(Socket),
			throw({temporary_failure, Msg});
		{ok, Msg} ->
			quit(Socket),
			throw({permanant_failure, Msg})
	end;
try_RCPT_TO([To | Tail], Socket, Extensions) ->
	% someone was bad and didn't put in the angle brackets
	try_RCPT_TO(["<"++To++">" | Tail], Socket, Extensions).

try_DATA(Body, Socket, _Extensions) ->
	socket:send(Socket, "DATA\r\n"),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"354", _Rest/binary>>} ->
			socket:send(Socket, [Body, "\r\n.\r\n"]),
			case read_possible_multiline_reply(Socket) of
				{ok, <<"250", Receipt/binary>>} ->
					Receipt;
				{ok, <<"4", _Rest2/binary>> = Msg} ->
					quit(Socket),
					throw({temporary_failure, Msg});
				{ok, Msg} ->
					quit(Socket),
					throw({permanant_failure, Msg})
			end;
		{ok, <<"4", _Rest/binary>> = Msg} ->
			quit(Socket),
			throw({temporary_failure, Msg});
		{ok, Msg} ->
			quit(Socket),
			throw({permanant_failure, Msg})
	end.

try_AUTH(Socket, Options, []) ->
	case proplists:get_value(auth, Options) of
		always ->
			quit(Socket),
			erlang:throw({missing_requirement, auth});
		_ ->
			false
	end;
try_AUTH(Socket, Options, undefined) ->
	case proplists:get_value(auth, Options) of
		always ->
			quit(Socket),
			erlang:throw({missing_requirement, auth});
		_ ->
			false
	end;
try_AUTH(Socket, Options, AuthTypes) ->
	case proplists:is_defined(username, Options) and
		proplists:is_defined(password, Options) and
		(proplists:get_value(auth, Options) =/= never) of
		false ->
			case proplists:get_value(auth, Options) of
				always ->
					quit(Socket),
					erlang:throw({missing_requirement, auth});
				_ ->
					false
			end;
		true ->
			Username = proplists:get_value(username, Options),
			Password = proplists:get_value(password, Options),
			%io:format("Auth types: ~p~n", [AuthTypes]),
			Types = re:split(AuthTypes, " ", [{return, list}, trim]),
			case do_AUTH(Socket, Username, Password, Types) of
				false ->
					case proplists:get_value(auth, Options) of
						always ->
							quit(Socket),
							erlang:throw({permanant_failure, auth_failed});
						_ ->
							false
					end;
				true ->
					true
			end
	end.

do_AUTH(Socket, Username, Password, Types) ->
	FixedTypes = [string:to_upper(X) || X <- Types],
	%io:format("Fixed types: ~p~n", [FixedTypes]),
	AllowedTypes = [X  || X <- ?AUTH_PREFERENCE, lists:member(X, FixedTypes)],
	%io:format("available authentication types, in order of preference: ~p~n",
	%	[AllowedTypes]),
	do_AUTH_each(Socket, Username, Password, AllowedTypes).

do_AUTH_each(_Socket, _Username, _Password, []) ->
	false;
do_AUTH_each(Socket, Username, Password, ["CRAM-MD5" | Tail]) ->
	socket:send(Socket, "AUTH CRAM-MD5\r\n"),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"334 ", Rest/binary>>} ->
			Seed64 = binstr:strip(binstr:strip(Rest, right, $\n), right, $\r),
			Seed = base64:decode_to_string(Seed64),
			Digest = smtp_util:compute_cram_digest(Password, Seed),
			String = base64:encode(list_to_binary([Username, " ", Digest])),
			socket:send(Socket, [String, "\r\n"]),
			case read_possible_multiline_reply(Socket) of
				{ok, <<"235", _Rest/binary>>} ->
					%io:format("authentication accepted~n"),
					true;
				{ok, _Msg} ->
					%io:format("authentication rejected: ~s~n", [Msg]),
					do_AUTH_each(Socket, Username, Password, Tail)
			end;
		{ok, _Something} ->
			%io:format("got ~s~n", [Something]),
			do_AUTH_each(Socket, Username, Password, Tail)
	end;
do_AUTH_each(Socket, Username, Password, ["LOGIN" | Tail]) ->
	socket:send(Socket, "AUTH LOGIN\r\n"),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"334 VXNlcm5hbWU6\r\n">>} ->
			%io:format("username prompt~n"),
			U = base64:encode(Username),
			socket:send(Socket, [U,"\r\n"]),
			case read_possible_multiline_reply(Socket) of
				{ok, <<"334 UGFzc3dvcmQ6\r\n">>} ->
					%io:format("password prompt~n"),
					P = base64:encode(Password),
					socket:send(Socket, [P,"\r\n"]),
					case read_possible_multiline_reply(Socket) of
						{ok, <<"235 ", _Rest/binary>>} ->
							%io:format("authentication accepted~n"),
							true;
						{ok, _Msg} ->
							%io:format("password rejected: ~s", [Msg]),
							do_AUTH_each(Socket, Username, Password, Tail)
					end;
				{ok, _Msg2} ->
					%io:format("username rejected: ~s", [Msg2]),
					do_AUTH_each(Socket, Username, Password, Tail)
			end;
		{ok, _Something} ->
			%io:format("got ~s~n", [Something]),
			do_AUTH_each(Socket, Username, Password, Tail)
	end;
do_AUTH_each(Socket, Username, Password, ["PLAIN" | Tail]) ->
	AuthString = base64:encode("\0"++Username++"\0"++Password),
	socket:send(Socket, ["AUTH PLAIN ", AuthString, "\r\n"]),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"235", _Rest/binary>>} ->
			%io:format("authentication accepted~n"),
			true;
		_Else ->
			% TODO do we need to bother trying the multi-step PLAIN?
			%io:format("authentication rejected~n"),
			%io:format("~p~n", [Else]),
			do_AUTH_each(Socket, Username, Password, Tail)
	end;
do_AUTH_each(Socket, Username, Password, [_Type | Tail]) ->
	%io:format("unsupported AUTH type ~s~n", [Type]),
	do_AUTH_each(Socket, Username, Password, Tail).

try_EHLO(Socket, Options) ->
	ok = socket:send(Socket, ["EHLO ", proplists:get_value(hostname, Options, smtp_util:guess_FQDN()), "\r\n"]),
	{ok, Reply} = read_possible_multiline_reply(Socket),
	{ok, parse_extensions(Reply)}.

% check if we should try to do TLS
try_STARTTLS(Socket, Options, Extensions) ->
		case {proplists:get_value(tls, Options),
				proplists:get_value(<<"STARTTLS">>, Extensions)} of
			{Atom, true} when Atom =:= always; Atom =:= if_available ->
			%io:format("Starting TLS~n"),
			case {do_STARTTLS(Socket, Options), Atom} of
				{false, always} ->
					%io:format("TLS failed~n"),
					quit(Socket),
					erlang:throw({temporary_failure, tls_failed});
				{false, if_available} ->
					%io:format("TLS failed~n"),
					{Socket, Extensions};
				{{S, E}, _} ->
					%io:format("TLS started~n"),
					{S, E}
			end;
		{always, _} ->
			quit(Socket),
			erlang:throw({missing_requirement, tls});
		_ ->
			{Socket, Extensions}
	end.

%% attempt to upgrade socket to TLS
do_STARTTLS(Socket, Options) ->
	socket:send(Socket, "STARTTLS\r\n"),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"220", _Rest/binary>>} ->
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			case socket:to_ssl_client(Socket, [], 5000) of
				{ok, NewSocket} ->
					%NewSocket;
					{ok, Extensions} = try_EHLO(NewSocket, Options),
					{NewSocket, Extensions};
				_Else ->
					%io:format("~p~n", [Else]),
					false
			end;
		{ok, <<"4", _Rest/binary>> = Msg} ->
			quit(Socket),
			throw({temporary_failure, Msg});
		{ok, Msg} ->
			quit(Socket),
			throw({permanant_failure, Msg})
	end.

%% try connecting to a host
connect(Host, Options) when is_binary(Host) ->
	connect(binary_to_list(Host), Options);
connect(Host, Options) ->
	SockOpts = [binary, {packet, line}, {keepalive, true}, {active, false}],
	Proto = case proplists:get_value(ssl, Options) of
		true ->
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			ssl;
		_ ->
			tcp
	end,
	Port = case proplists:get_value(port, Options) of
		undefined when Proto =:= ssl ->
			465;
		OPort when is_integer(OPort) ->
			OPort;
		_ ->
			25
	end,
	case socket:connect(Proto, Host, Port, SockOpts, 5000) of
		{ok, Socket} ->
			case read_possible_multiline_reply(Socket) of
				{ok, <<"220", Banner/binary>>} ->
					{ok, Socket, Host, Banner};
				{ok, <<"4", _Rest/binary>> = Msg} ->
					quit(Socket),
					throw({temporary_failure, Msg});
				{ok, Msg} ->
					quit(Socket),
					throw({permanant_failure, Msg})
			end;
		{error, Reason} ->
			throw({network_failure, {error, Reason}})
	end.

%% read a multiline reply (eg. EHLO reply)
read_possible_multiline_reply(Socket) ->
	case socket:recv(Socket, 0, ?TIMEOUT) of
		{ok, Packet} ->
			case binstr:substr(Packet, 4, 1) of
				<<"-">> ->
					Code = binstr:substr(Packet, 1, 3),
					read_multiline_reply(Socket, Code, [Packet]);
				<<" ">> ->
					{ok, Packet}
			end;
		Error ->
			throw({network_failure, Error})
	end.

read_multiline_reply(Socket, Code, Acc) ->
	case socket:recv(Socket, 0, ?TIMEOUT) of
		{ok, Packet} ->
			case {binstr:substr(Packet, 1, 3), binstr:substr(Packet, 4, 1)} of
				{Code, <<" ">>} ->
					{ok, list_to_binary(lists:reverse([Packet | Acc]))};
				{Code, <<"-">>} ->
					read_multiline_reply(Socket, Code, [Packet | Acc]);
				_ ->
					quit(Socket),
					throw({unexpected_response, lists:reverse([Packet | Acc])})
			end;
		Error ->
			throw({network_failure, Error})
	end.

quit(Socket) ->
	socket:send(Socket, "QUIT\r\n"),
	socket:close(Socket),
	ok.

% TODO - more checking
check_options(Options) ->
	case proplists:get_value(relay, Options) of
		undefined ->
			{error, no_relay};
		_ ->
			case proplists:get_value(auth, Options) of
				Atom when Atom =:= always ->
					case proplists:is_defined(username, Options) and
						proplists:is_defined(password, Options) of
						false ->
							{error, no_credentials};
						true ->
							ok
					end;
				_ ->
					ok
			end
	end.

parse_extensions(Reply) ->
	[_ | Reply2] = re:split(Reply, "\r\n", [{return, binary}, trim]),
	[
		begin
				Body = binstr:substr(Entry, 5),
				case re:split(Body, " ",  [{return, binary}, trim, {parts, 2}]) of
					[Verb, Parameters] ->
						{binstr:to_upper(Verb), Parameters};
					[Body] ->
						case binstr:strchr(Body, $=) of
							0 ->
								{binstr:to_upper(Body), true};
							_ ->
								%io:format("discarding option ~p~n", [Body]),
								[]
						end
				end
		end  || Entry <- Reply2].

-ifdef(TEST).

session_start_test_() ->
	{foreach,
		local,
		fun() ->
				{ok, ListenSock} = socket:listen(tcp, 9876),
				{ListenSock}
		end,
		fun({ListenSock}) ->
				socket:close(ListenSock)
		end,
		[fun({ListenSock}) ->
					{"simple session initiation",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"retry on crashed EHLO twice if requested",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {retries, 2}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:close(X),
								{ok, Y} = socket:accept(ListenSock, 1000),
								socket:send(Y, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
								socket:close(Y),
								{ok, Z} = socket:accept(ListenSock, 1000),
								socket:send(Z, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Z, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"retry on crashed EHLO",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
								{ok, Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								unlink(Pid),
								Monitor = erlang:monitor(process, Pid),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:close(X),
								{ok, Y} = socket:accept(ListenSock, 1000),
								socket:send(Y, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
								socket:close(Y),
								?assertEqual({error, timeout}, socket:accept(ListenSock, 1000)),
								receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, retries_exceeded, _}, Error) end,
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"abort on 554 greeting",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
								{ok, Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								unlink(Pid),
								Monitor = erlang:monitor(process, Pid),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "554 get lost, kid\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, no_more_hosts, _}, Error) end,
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"retry on 421 greeting",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "421 can't you see I'm busy?\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								{ok, Y} = socket:accept(ListenSock, 1000),
								socket:send(Y, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"retry on messed up EHLO response",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
								{ok, Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								unlink(Pid),
								Monitor = erlang:monitor(process, Pid),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-server.example.com EHLO\r\n250-AUTH LOGIN PLAIN\r\n421 too busy\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								
								{ok, Y} = socket:accept(ListenSock, 1000),
								socket:send(Y, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250-server.example.com EHLO\r\n250-AUTH LOGIN PLAIN\r\n421 too busy\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(Y, 0, 1000)),
								receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, retries_exceeded, _}, Error) end,
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"a valid complete transaction without TLS advertised should succeed",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 hostname\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "RCPT TO: <foo@bar.com>\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "DATA\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "354 ok\r\n"),
								?assertMatch({ok, "hello world\r\n"}, socket:recv(X, 0, 1000)),
								?assertMatch({ok, ".\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"a valid complete transaction with binary arguments shoyld succeed",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
								{ok, _Pid} = send({<<"test@foo.com">>, [<<"foo@bar.com">>], <<"hello world">>}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 hostname\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "RCPT TO: <foo@bar.com>\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "DATA\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "354 ok\r\n"),
								?assertMatch({ok, "hello world\r\n"}, socket:recv(X, 0, 1000)),
								?assertMatch({ok, ".\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"a valid complete transaction with TLS advertised should succeed",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, <<"testing">>}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 STARTTLS\r\n"),
								?assertMatch({ok, "STARTTLS\r\n"}, socket:recv(X, 0, 1000)),
								application:start(crypto),
								application:start(public_key),
								application:start(ssl),
								socket:send(X, "220 ok\r\n"),
								{ok, Y} = socket:to_ssl_server(X, [{certfile, "../testdata/server.crt"}, {keyfile, "../testdata/server.key"}], 5000),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250-hostname\r\n250 STARTTLS\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250 ok\r\n"),
								?assertMatch({ok, "RCPT TO: <foo@bar.com>\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250 ok\r\n"),
								?assertMatch({ok, "DATA\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "354 ok\r\n"),
								?assertMatch({ok, "hello world\r\n"}, socket:recv(Y, 0, 1000)),
								?assertMatch({ok, ".\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250 ok\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(Y, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"a valid complete transaction with TLS advertised and binary arguments should succeed",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, <<"testing">>}],
								{ok, _Pid} = send({<<"test@foo.com">>, [<<"foo@bar.com">>], <<"hello world">>}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 STARTTLS\r\n"),
								?assertMatch({ok, "STARTTLS\r\n"}, socket:recv(X, 0, 1000)),
								application:start(crypto),
								application:start(public_key),
								application:start(ssl),
								socket:send(X, "220 ok\r\n"),
								{ok, Y} = socket:to_ssl_server(X, [{certfile, "../testdata/server.crt"}, {keyfile, "../testdata/server.key"}], 5000),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250-hostname\r\n250 STARTTLS\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250 ok\r\n"),
								?assertMatch({ok, "RCPT TO: <foo@bar.com>\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250 ok\r\n"),
								?assertMatch({ok, "DATA\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "354 ok\r\n"),
								?assertMatch({ok, "hello world\r\n"}, socket:recv(Y, 0, 1000)),
								?assertMatch({ok, ".\r\n"}, socket:recv(Y, 0, 1000)),
								socket:send(Y, "250 ok\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(Y, 0, 1000)),
								ok
						end
					}
			end,

			fun({ListenSock}) ->
					{"AUTH PLAIN should work",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {username, "user"}, {password, "pass"}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 AUTH PLAIN\r\n"),
								AuthString = binary_to_list(base64:encode("\0user\0pass")),
								AuthPacket = "AUTH PLAIN "++AuthString++"\r\n",
								?assertEqual({ok, AuthPacket}, socket:recv(X, 0, 1000)),
								socket:send(X, "235 ok\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"AUTH LOGIN should work",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {username, "user"}, {password, "pass"}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 AUTH LOGIN\r\n"),
								?assertEqual({ok, "AUTH LOGIN\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "334 VXNlcm5hbWU6\r\n"),
								UserString = binary_to_list(base64:encode("user")),
								?assertEqual({ok, UserString++"\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "334 UGFzc3dvcmQ6\r\n"),
								PassString = binary_to_list(base64:encode("pass")),
								?assertEqual({ok, PassString++"\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "235 ok\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"AUTH CRAM-MD5 should work",
						fun() ->
								Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {username, "user"}, {password, "pass"}],
								{ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 AUTH CRAM-MD5\r\n"),
								?assertEqual({ok, "AUTH CRAM-MD5\r\n"}, socket:recv(X, 0, 1000)),
								Seed = smtp_util:get_cram_string(smtp_util:guess_FQDN()),
								DecodedSeed = base64:decode_to_string(Seed),
								Digest = smtp_util:compute_cram_digest("pass", DecodedSeed),
								String = binary_to_list(base64:encode(list_to_binary(["user ", Digest]))),
								socket:send(X, "334 "++Seed++"\r\n"),
								{ok, Packet} = socket:recv(X, 0, 1000),
								CramDigest = smtp_util:trim_crlf(Packet),
								?assertEqual(String, CramDigest),
								socket:send(X, "235 ok\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"AUTH CRAM-MD5 should work",
						fun() ->
								Options = [{relay, <<"localhost">>}, {port, 9876}, {hostname, <<"testing">>}, {username, <<"user">>}, {password, <<"pass">>}],
								{ok, _Pid} = send({<<"test@foo.com">>, [<<"foo@bar.com">>, <<"baz@bar.com">>], <<"hello world">>}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 AUTH CRAM-MD5\r\n"),
								?assertEqual({ok, "AUTH CRAM-MD5\r\n"}, socket:recv(X, 0, 1000)),
								Seed = smtp_util:get_cram_string(smtp_util:guess_FQDN()),
								DecodedSeed = base64:decode_to_string(Seed),
								Digest = smtp_util:compute_cram_digest("pass", DecodedSeed),
								String = binary_to_list(base64:encode(list_to_binary(["user ", Digest]))),
								socket:send(X, "334 "++Seed++"\r\n"),
								{ok, Packet} = socket:recv(X, 0, 1000),
								CramDigest = smtp_util:trim_crlf(Packet),
								?assertEqual(String, CramDigest),
								socket:send(X, "235 ok\r\n"),
								?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"should bail when AUTH is required but not provided",
						fun() ->
								Options = [{relay, <<"localhost">>}, {port, 9876}, {hostname, <<"testing">>}, {auth, always}, {username, <<"user">>}, {retries, 0}, {password, <<"pass">>}],
								{ok, Pid} = send({<<"test@foo.com">>, [<<"foo@bar.com">>, <<"baz@bar.com">>], <<"hello world">>}, Options),
								unlink(Pid),
								Monitor = erlang:monitor(process, Pid),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 8BITMIME\r\n"),
								?assertEqual({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, retries_exceeded, {missing_requirement, _, auth}}, Error) end,
								ok
						end
					}
			end,
			fun({ListenSock}) ->
					{"should bail when AUTH is required but of an unsupported type",
						fun() ->
								Options = [{relay, <<"localhost">>}, {port, 9876}, {hostname, <<"testing">>}, {auth, always}, {username, <<"user">>}, {retries, 0}, {password, <<"pass">>}],
								{ok, Pid} = send({<<"test@foo.com">>, [<<"foo@bar.com">>, <<"baz@bar.com">>], <<"hello world">>}, Options),
								unlink(Pid),
								Monitor = erlang:monitor(process, Pid),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250-AUTH GSSAPI\r\n250 8BITMIME\r\n"),
								?assertEqual({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, no_more_hosts, {permanant_failure, _, auth_failed}}, Error) end,
								ok
						end
					}
			end,
			fun({_ListenSock}) ->
					{"Connecting to a SSL socket directly should work",
						fun() ->
								application:start(crypto),
								application:start(public_key),
								application:start(ssl),
								{ok, ListenSock} = socket:listen(ssl, 9877, [{certfile, "../testdata/server.crt"}, {keyfile, "../testdata/server.key"}]),
								Options = [{relay, <<"localhost">>}, {port, 9877}, {hostname, <<"testing">>}, {ssl, true}],
								{ok, _Pid} = send({<<"test@foo.com">>, [<<"<foo@bar.com>">>, <<"baz@bar.com">>], <<"hello world">>}, Options),
								{ok, X} = socket:accept(ListenSock, 1000),
								socket:send(X, "220 Some banner\r\n"),
								?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250-hostname\r\n250 AUTH CRAM-MD5\r\n"),
								?assertEqual({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "RCPT TO: <foo@bar.com>\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "RCPT TO: <baz@bar.com>\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "DATA\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "354 ok\r\n"),
								?assertMatch({ok, "hello world\r\n"}, socket:recv(X, 0, 1000)),
								?assertMatch({ok, ".\r\n"}, socket:recv(X, 0, 1000)),
								socket:send(X, "250 ok\r\n"),
								?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
								socket:close(ListenSock),
								ok
						end
					}
			end

		]
	}.

extension_parse_test_() ->
	[
		{"parse extensions",
			fun() ->
					Res = parse_extensions(<<"250-smtp.example.com\r\n250-PIPELINING\r\n250-SIZE 20971520\r\n250-VRFY\r\n250-ETRN\r\n250-STARTTLS\r\n250-AUTH CRAM-MD5 PLAIN DIGEST-MD5 LOGIN\r\n250-AUTH=CRAM-MD5 PLAIN DIGEST-MD5 LOGIN\r\n250-ENHANCEDSTATUSCODES\r\n250-8BITMIME\r\n250 DSN">>),
					?assertEqual(true, proplists:get_value(<<"PIPELINING">>, Res)),
					?assertEqual(<<"20971520">>, proplists:get_value(<<"SIZE">>, Res)),
					?assertEqual(true, proplists:get_value(<<"VRFY">>, Res)),
					?assertEqual(true, proplists:get_value(<<"ETRN">>, Res)),
					?assertEqual(true, proplists:get_value(<<"STARTTLS">>, Res)),
					?assertEqual(<<"CRAM-MD5 PLAIN DIGEST-MD5 LOGIN">>, proplists:get_value(<<"AUTH">>, Res)),
					?assertEqual(true, proplists:get_value(<<"ENHANCEDSTATUSCODES">>, Res)),
					?assertEqual(true, proplists:get_value(<<"8BITMIME">>, Res)),
					?assertEqual(true, proplists:get_value(<<"DSN">>, Res)),
					?assertEqual(10, length(Res)),
					ok
			end
		}
	].

-endif.
