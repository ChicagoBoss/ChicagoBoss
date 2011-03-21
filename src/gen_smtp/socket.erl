%%% Copyright 2009 Jack Danger Canty <code@jackcanty.com>. All rights reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to
%%% permit persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

%% @doc Facilitates transparent gen_tcp/ssl socket handling
-module(socket).


-define(TCP_LISTEN_OPTIONS,[  {active, false},
                              {backlog, 30},
                              {ip,{0,0,0,0}},
                              {keepalive, true},
                              {packet, line},
                              {reuseaddr, true}]).
-define(TCP_CONNECT_OPTIONS,[ {active, false},
                              {packet, line}]).
-define(SSL_LISTEN_OPTIONS, [ {active, false},
                              {backlog, 30},
                              {certfile, "server.crt"},
                              {depth, 0},
                              {keepalive, true},
                              {keyfile, "server.key"},
                              {packet, line},
                              {reuse_sessions, false},
                              {reuseaddr, true},
                              {ssl_imp, new}]).
-define(SSL_CONNECT_OPTIONS,[ {active, false},
                              {depth, 0},
                              {packet, line},
                              {ssl_imp, new}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([connect/3, connect/4, connect/5]).
-export([listen/2, listen/3, accept/1, accept/2]).
-export([send/2, recv/2, recv/3]).
-export([controlling_process/2]).
-export([peername/1]).
-export([close/1, shutdown/2]).
-export([active_once/1]).
-export([setopts/2]).
-export([get_proto/1]).
-export([begin_inet_async/1]).
-export([handle_inet_async/1, handle_inet_async/2, handle_inet_async/3]).
-export([extract_port_from_socket/1]).
-export([to_ssl_server/1,to_ssl_server/2,to_ssl_server/3]).
-export([to_ssl_client/1,to_ssl_client/2,to_ssl_client/3]).
-export([type/1]).

-type protocol() :: 'tcp' | 'ssl'.
-type address() :: inet:ip_address() | string() | binary().
-type socket() :: ssl:sslsocket() | gen_tcp:socket().

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------
-spec connect(Protocol :: protocol(), Address :: address(), Port :: pos_integer()) -> socket().
connect(Protocol, Address, Port) ->
	connect(Protocol, Address, Port, [], infinity).

-spec connect(Protocol :: protocol(), Address :: address(), Port :: pos_integer(), Options :: list()) -> socket().
connect(Protocol, Address, Port, Opts) ->
	connect(Protocol, Address, Port, Opts, infinity).

-spec connect(Protocol :: protocol(), Address :: address(), Port :: pos_integer(), Options :: list(), Time :: non_neg_integer() | 'infinity') -> socket().
connect(tcp, Address, Port, Opts, Time) ->
	gen_tcp:connect(Address, Port, tcp_connect_options(Opts), Time);
connect(ssl, Address, Port, Opts, Time) ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	ssl:connect(Address, Port, ssl_connect_options(Opts), Time).


-spec listen(Protocol :: protocol(), Port :: pos_integer()) -> socket().
listen(Protocol, Port) ->
	listen(Protocol, Port, []).

-spec listen(Protocol :: protocol(), Port :: pos_integer(), Options :: list()) -> socket().
listen(ssl, Port, Options) ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	ssl:listen(Port, ssl_listen_options(Options));
listen(tcp, Port, Options) ->
	gen_tcp:listen(Port, tcp_listen_options(Options)).


-spec accept(Socket :: socket()) -> {'ok', socket()} | {'error', any()}.
accept(Socket) ->
	accept(Socket, infinity).

-spec accept(Socket :: socket(), Timeout :: pos_integer() | 'infinity') -> {'ok', socket()} | {'error', any()}.
accept(Socket, Timeout) when is_port(Socket) ->
	case gen_tcp:accept(Socket, Timeout) of
		{ok, NewSocket} ->
			{ok, Opts} = inet:getopts(Socket, [active,keepalive,packet,reuseaddr]),
			inet:setopts(NewSocket, Opts),
			{ok, NewSocket};
		Error ->
			Error
	end;
accept(Socket, Timeout) ->
	case ssl:transport_accept(Socket, Timeout) of
		{ok, NewSocket} ->
			ssl:ssl_accept(NewSocket),
			{ok, NewSocket};
		Error -> Error
	end.

-spec send(Socket :: socket(), Data :: binary() | string()) -> 'ok' | {'error', any()}.
send(Socket, Data) when is_port(Socket) ->
	gen_tcp:send(Socket, Data);
send(Socket, Data) ->
	ssl:send(Socket, Data).

-spec recv(Socket :: socket(), Length :: non_neg_integer()) -> {'ok', any()} | {'error', any()}.
recv(Socket, Length) ->
	recv(Socket, Length, infinity).

-spec recv(Socket :: socket(), Length :: non_neg_integer(), Timeout :: non_neg_integer() | 'infinity') -> {'ok', any()} | {'error', any()}.
recv(Socket, Length, Timeout) when is_port(Socket) ->
	gen_tcp:recv(Socket, Length, Timeout);
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec controlling_process(Socket :: socket(), NewOwner :: pid()) -> 'ok' | {'error', any()}.
controlling_process(Socket, NewOwner) when is_port(Socket) ->
	gen_tcp:controlling_process(Socket, NewOwner);
controlling_process(Socket, NewOwner) ->
	ssl:controlling_process(Socket, NewOwner).

-spec peername(Socket :: socket()) -> {ok, {inet:ip_address(), non_neg_integer()}} | {'error', any()}.
peername(Socket) when is_port(Socket) ->
	inet:peername(Socket);
peername(Socket) ->
	ssl:peername(Socket).

-spec close(Socket :: socket()) -> 'ok'.
close(Socket) when is_port(Socket) ->
	gen_tcp:close(Socket);
close(Socket) ->
	ssl:close(Socket).

-spec shutdown(Socket :: socket(), How :: 'read' | 'write' | 'read_write') -> 'ok' | {'error', any()}.
shutdown(Socket, How) when is_port(Socket) ->
	gen_tcp:shutdown(Socket, How);
shutdown(Socket, How) ->
	ssl:shutdown(Socket, How).

-spec active_once(Socket :: socket()) -> 'ok' | {'error', any()}.
active_once(Socket) when is_port(Socket) ->
	inet:setopts(Socket, [{active, once}]);
active_once(Socket) ->
	ssl:setopts(Socket, [{active, once}]).

-spec setopts(Socket :: socket(), Options :: list()) -> 'ok' | {'error', any()}.
setopts(Socket, Options) when is_port(Socket) ->
	inet:setopts(Socket, Options);
setopts(Socket, Options) ->
	ssl:setopts(Socket, Options).

-spec(get_proto/1 :: (Socket :: any()) -> 'tcp' | 'ssl').
get_proto(Socket) when is_port(Socket) ->
	tcp;
get_proto(_Socket) ->
	ssl.

%% @doc {inet_async,...} will be sent to current process when a client connects
-spec begin_inet_async(Socket :: socket()) -> any().
begin_inet_async(Socket) when is_port(Socket) ->
	prim_inet:async_accept(Socket, -1);
begin_inet_async(Socket) ->
	Port = extract_port_from_socket(Socket),
	begin_inet_async(Port).

%% @doc handle the {inet_async,...} message
-spec handle_inet_async(Message :: {'inet_async', socket(), any(), {'ok', socket()}}) -> {'ok', socket()}.
handle_inet_async({inet_async, ListenSocket, _, {ok,ClientSocket}}) ->
	handle_inet_async(ListenSocket, ClientSocket, []).

-spec handle_inet_async(ListenSocket :: socket(), ClientSocket :: socket()) -> {'ok', socket()}.
handle_inet_async(ListenObject, ClientSocket) ->
	handle_inet_async(ListenObject, ClientSocket, []).

-spec handle_inet_async(ListenSocket :: socket(), ClientSocket :: socket(), Options :: list()) -> {'ok', socket()}.
handle_inet_async(ListenObject, ClientSocket, Options) ->
	ListenSocket = extract_port_from_socket(ListenObject),
	case set_sockopt(ListenSocket, ClientSocket) of
		ok -> ok;
		Error -> erlang:error(set_sockopt, Error)
	end,
	%% Signal the network driver that we are ready to accept another connection
	begin_inet_async(ListenSocket),
	%% If the listening socket is SSL then negotiate the client socket
	case is_port(ListenObject) of
		true ->
			{ok, ClientSocket};
		false ->
			{ok, UpgradedClientSocket} = to_ssl_server(ClientSocket, Options),
			{ok, UpgradedClientSocket}
	end.

%% @doc Upgrade a TCP connection to SSL
-spec to_ssl_server(Socket :: socket()) -> {'ok', ssl:socket()} | {'error', any()}.
to_ssl_server(Socket) ->
	to_ssl_server(Socket, []).

-spec to_ssl_server(Socket :: socket(), Options :: list()) -> {'ok', ssl:socket()} | {'error', any()}.
to_ssl_server(Socket, Options) ->
	to_ssl_server(Socket, Options, infinity).

-spec to_ssl_server(Socket :: socket(), Options :: list(), Timeout :: non_neg_integer() | 'infinity') -> {'ok', ssl:socket()} | {'error', any()}.
to_ssl_server(Socket, Options, Timeout) when is_port(Socket) ->
	ssl:ssl_accept(Socket, ssl_listen_options(Options), Timeout);
to_ssl_server(_Socket, _Options, _Timeout) ->
	erlang:error(ssl_connected, "Socket is already using SSL").

-spec to_ssl_client(Socket :: socket()) -> {'ok', ssl:sslsocket()} | {'error', any()}.
to_ssl_client(Socket) ->
	to_ssl_client(Socket, []).

-spec to_ssl_client(Socket :: socket(), Options :: list()) -> {'ok', ssl:sslsocket()} | {'error', any()}.
to_ssl_client(Socket, Options) ->
	to_ssl_client(Socket, Options, infinity).

-spec to_ssl_client(Socket :: socket(), Options :: list(), Timeout :: non_neg_integer() | 'infinity') -> {'ok', ssl:sslsocket()} | {'error', any()}.
to_ssl_client(Socket, Options, Timeout) when is_port(Socket) ->
	ssl:connect(Socket, ssl_connect_options(Options), Timeout);
to_ssl_client(_Socket, _Options, _Timeout) ->
	erlang:error(ssl_connected, "Socket is already using SSL").

-spec type(Socket :: socket()) -> protocol().
type(Socket) when is_port(Socket) ->
	tcp;
type(_Socket) ->
	ssl.

%%%-----------------------------------------------------------------
%%% Internal functions (OS_Mon configuration)
%%%-----------------------------------------------------------------

tcp_listen_options([Format|Options]) when Format =:= list; Format =:= binary ->
	tcp_listen_options(Options, Format);
tcp_listen_options(Options) ->
	tcp_listen_options(Options, list).
tcp_listen_options(Options, Format) ->
	parse_address([Format|proplist_merge(Options, ?TCP_LISTEN_OPTIONS)]).

ssl_listen_options([Format|Options]) when Format =:= list; Format =:= binary ->
	ssl_listen_options(Options, Format);
ssl_listen_options(Options) ->
	ssl_listen_options(Options, list).
ssl_listen_options(Options, Format) ->
	parse_address([Format|proplist_merge(Options, ?SSL_LISTEN_OPTIONS)]).

tcp_connect_options([Format|Options]) when Format =:= list; Format =:= binary ->
	tcp_connect_options(Options, Format);
tcp_connect_options(Options) ->
	tcp_connect_options(Options, list).
tcp_connect_options(Options, Format) ->
	parse_address([Format|proplist_merge(Options, ?TCP_CONNECT_OPTIONS)]).

ssl_connect_options([Format|Options]) when Format =:= list; Format =:= binary ->
	ssl_connect_options(Options, Format);
ssl_connect_options(Options) ->
	ssl_connect_options(Options, list).
ssl_connect_options(Options, Format) ->
	parse_address([Format|proplist_merge(Options, ?SSL_CONNECT_OPTIONS)]).

proplist_merge(PrimaryList, DefaultList) ->
	{PrimaryTuples, PrimaryOther} = lists:partition(fun(X) -> is_tuple(X) end, PrimaryList),
	{DefaultTuples, DefaultOther} = lists:partition(fun(X) -> is_tuple(X) end, DefaultList),
	MergedTuples = lists:ukeymerge(1,
		lists:keysort(1, PrimaryTuples),
		lists:keysort(1, DefaultTuples)
	),
	MergedOther = lists:merge(lists:sort(PrimaryOther), lists:sort(DefaultOther)),

	%% remove all the values that don't belong here
	[Option  || Option = {Key, _} <- MergedTuples, proplists:is_defined(Key, DefaultList)] ++ [Option || Option <- MergedOther, Option == inet6 ].

parse_address(Options) ->
	case proplists:get_value(ip, Options) of
		X when is_tuple(X) ->
			Options;
		X when is_list(X) ->
			case inet_parse:address(X) of
				{error, _} = Error ->
					erlang:error(Error);
				{ok, IP} ->
					proplists:delete(ip, Options) ++ [{ip, IP}]
			end;
		_ ->
			Options
	end.

-spec extract_port_from_socket(Socket :: socket()) -> port().
extract_port_from_socket({sslsocket,_,{SSLPort,_}}) ->
	SSLPort;
extract_port_from_socket(Socket) ->
	Socket.

-spec(set_sockopt/2 :: (ListSock :: port(), CliSocket :: port()) -> 'ok' | any()).
set_sockopt(ListenObject, ClientSocket) ->
	ListenSocket = extract_port_from_socket(ListenObject),
	true = inet_db:register_socket(ClientSocket, inet_tcp),
	case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(ClientSocket, Opts) of
				ok -> ok;
				Error -> socket:close(ClientSocket), Error
			end;
		Error -> socket:close(ClientSocket), Error
	end.

-ifdef(TEST).
-define(TEST_PORT, 7586).

connect_test_() ->
	[
		{"listen and connect via tcp",
		fun() ->
			Self = self(),
			spawn(fun() ->
						{ok, ListenSocket} = listen(tcp, ?TEST_PORT),
						?assert(is_port(ListenSocket)),
						{ok, ServerSocket} = accept(ListenSocket),
						controlling_process(ServerSocket, Self),
						Self ! ListenSocket
				end),
			{ok, ClientSocket} = connect(tcp, "localhost", ?TEST_PORT),
			receive ListenSocket when is_port(ListenSocket) -> ok end,
			?assert(is_port(ClientSocket)),
			close(ListenSocket)
		end
		},
		{"listen and connect via ssl",
		fun() ->
			Self = self(),
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			spawn(fun() ->
						{ok, ListenSocket} = listen(ssl, ?TEST_PORT, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
						?assertMatch([sslsocket|_], tuple_to_list(ListenSocket)),
						{ok, ServerSocket} = accept(ListenSocket),
						controlling_process(ServerSocket, Self),
						Self ! ListenSocket
				end),
			{ok, ClientSocket} = connect(ssl, "localhost", ?TEST_PORT,  []),
			receive {sslsocket,_,_} = ListenSocket -> ok end,
			?assertMatch([sslsocket|_], tuple_to_list(ClientSocket)),
			close(ListenSocket)
		end
		}
	].

evented_connections_test_() ->
	[
		{"current process receives connection to TCP listen sockets",
		fun() ->
			{ok, ListenSocket} = listen(tcp, ?TEST_PORT),
			begin_inet_async(ListenSocket),
			spawn(fun()-> connect(tcp, "localhost", ?TEST_PORT) end),
			receive
				{inet_async, ListenSocket, _, {ok,ServerSocket}} -> ok
			end,
			{ok, NewServerSocket} = handle_inet_async(ListenSocket, ServerSocket),
			?assert(is_port(ServerSocket)),
			?assertEqual(ServerSocket, NewServerSocket), %% only true for TCP
			?assert(is_port(ListenSocket)),
			% Stop the async
			spawn(fun()-> connect(tcp, "localhost", ?TEST_PORT) end),
			receive _Ignored -> ok end,
			close(NewServerSocket),
			close(ListenSocket)
		end
		},
		{"current process receives connection to SSL listen sockets",
		fun() ->
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			{ok, ListenSocket} = listen(ssl, ?TEST_PORT, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
			begin_inet_async(ListenSocket),
			spawn(fun()-> connect(ssl, "localhost", ?TEST_PORT) end),
			receive
				{inet_async, _ListenPort, _, {ok,ServerSocket}} -> ok
			end,
			{ok, NewServerSocket} = handle_inet_async(ListenSocket, ServerSocket, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
			?assert(is_port(ServerSocket)),
			?assertMatch([sslsocket|_], tuple_to_list(NewServerSocket)),
			?assertMatch([sslsocket|_], tuple_to_list(ListenSocket)),
			 %Stop the async
			spawn(fun()-> connect(ssl, "localhost", ?TEST_PORT) end),
			receive _Ignored -> ok end,
			close(ListenSocket),
			close(NewServerSocket),
			ok
		end
		},
		%% TODO: figure out if the following passes because
		%% of an incomplete test case or if this really is
		%% a magical feature where a single listener
		%% can respond to either ssl or tcp connections.
		{"current TCP listener receives SSL connection",
		fun() ->
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			{ok, ListenSocket} = listen(tcp, ?TEST_PORT),
			begin_inet_async(ListenSocket),
			spawn(fun()-> connect(ssl, "localhost", ?TEST_PORT) end),
			receive
				{inet_async, _ListenPort, _, {ok,ServerSocket}} -> ok
			end,
			{ok, ServerSocket} = handle_inet_async(ListenSocket, ServerSocket),
			?assert(is_port(ListenSocket)),
			?assert(is_port(ServerSocket)),
			{ok, NewServerSocket} = to_ssl_server(ServerSocket, [{certfile, "../testdata/server.crt"}, {keyfile, "../testdata/server.key"}]),
			?assertMatch([sslsocket|_], tuple_to_list(NewServerSocket)),
			% Stop the async
			spawn(fun()-> connect(ssl, "localhost", ?TEST_PORT) end),
			receive _Ignored -> ok end,
			close(ListenSocket),
			close(NewServerSocket)
		end
		}
	].

accept_test_() ->
	[
		{"Accept via tcp",
		fun() ->
			{ok, ListenSocket} = listen(tcp, ?TEST_PORT, tcp_listen_options([])),
			?assert(is_port(ListenSocket)),
			spawn(fun()-> connect(ssl, "localhost", ?TEST_PORT, tcp_connect_options([])) end),
			{ok, ServerSocket} = accept(ListenSocket),
			?assert(is_port(ListenSocket)),
 			close(ServerSocket),
			close(ListenSocket)
		end
		},
		{"Accept via ssl",
		fun() ->
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			{ok, ListenSocket} = listen(ssl, ?TEST_PORT, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
			?assertMatch([sslsocket|_], tuple_to_list(ListenSocket)),
			spawn(fun()->connect(ssl, "localhost", ?TEST_PORT) end),
			accept(ListenSocket),
			close(ListenSocket)
		end
		}
	].

type_test_() ->
	[
		{"a tcp socket returns 'tcp'",
		fun() ->
			{ok, ListenSocket} = listen(tcp, ?TEST_PORT),
			?assertMatch(tcp, type(ListenSocket)),
			close(ListenSocket)
		end
		},
		{"an ssl socket returns 'ssl'",
		fun() ->
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			{ok, ListenSocket} = listen(ssl, ?TEST_PORT, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
			?assertMatch(ssl, type(ListenSocket)),
			close(ListenSocket)
		end
		}
	].

active_once_test_() ->
	[
		{"socket is set to active:once on tcp",
		fun() ->
			{ok, ListenSocket} = listen(tcp, ?TEST_PORT, tcp_listen_options([])),
			?assertEqual({ok, [{active,false}]}, inet:getopts(ListenSocket, [active])),
			active_once(ListenSocket),
			?assertEqual({ok, [{active,once}]}, inet:getopts(ListenSocket, [active])),
			close(ListenSocket)
		end
		},
		{"socket is set to active:once on ssl",
		fun() ->
			{ok, ListenSocket} = listen(ssl, ?TEST_PORT, ssl_listen_options([{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}])),
			?assertEqual({ok, [{active,false}]}, ssl:getopts(ListenSocket, [active])),
			active_once(ListenSocket),
			?assertEqual({ok, [{active,once}]}, ssl:getopts(ListenSocket, [active])),
			close(ListenSocket)
		end
		}
	].

option_test_() ->
	[
		{"options removes bogus values",
		fun() ->
			?assertEqual([list|?TCP_LISTEN_OPTIONS], tcp_listen_options([{notvalid,whatever}]))
		end
		},
		{"tcp_listen_options has defaults",
		fun() ->
			?assertEqual([list|?TCP_LISTEN_OPTIONS], tcp_listen_options([]))
		end
		},
		{"tcp_connect_options has defaults",
		fun() ->
			?assertEqual([list|?TCP_CONNECT_OPTIONS], tcp_connect_options([]))
		end
		},
		{"ssl_listen_options has defaults",
		fun() ->
			?assertEqual([list|?SSL_LISTEN_OPTIONS], ssl_listen_options([]))
		end
		},
		{"ssl_connect_options has defaults",
		fun() ->
			?assertEqual([list|?SSL_CONNECT_OPTIONS], ssl_connect_options([]))
		end
		},
		{"tcp_listen_options defaults to list type",
		fun() ->
			?assertEqual([list|?TCP_LISTEN_OPTIONS], tcp_listen_options([{active,false}])),
			?assertEqual([binary|?TCP_LISTEN_OPTIONS], tcp_listen_options([binary,{active,false}]))
		end
		},
		{"tcp_connect_options defaults to list type",
		fun() ->
			?assertEqual([list|?TCP_CONNECT_OPTIONS], tcp_connect_options([{active,false}])),
			?assertEqual([binary|?TCP_CONNECT_OPTIONS], tcp_connect_options([binary,{active,false}]))
		end
		},
		{"ssl_listen_options defaults to list type",
		fun() ->
			?assertEqual([list|?SSL_LISTEN_OPTIONS], ssl_listen_options([{active,false}])),
			?assertEqual([binary|?SSL_LISTEN_OPTIONS], ssl_listen_options([binary,{active,false}]))
		end
		},
		{"ssl_connect_options defaults to list type",
		fun() ->
			?assertEqual([list|?SSL_CONNECT_OPTIONS], ssl_connect_options([{active,false}])),
			?assertEqual([binary|?SSL_CONNECT_OPTIONS], ssl_connect_options([binary,{active,false}]))
		end
		},
		{"tcp_listen_options merges provided proplist",
		fun() ->
			?assertMatch([list,{active, true},
			                   {backlog, 30},
			                   {ip,{0,0,0,0}},
			                   {keepalive, true},
			                   {packet, 2},
			                   {reuseaddr, true}],
			             tcp_listen_options([{active, true},{packet,2}]))
		end
		},
		{"tcp_connect_options merges provided proplist",
		fun() ->
			?assertMatch([list,{active, true},
			                   {packet, 2}],
			             tcp_connect_options([{active, true},{packet,2}]))
		end
		},
		{"ssl_listen_options merges provided proplist",
		fun() ->
			?assertMatch([list,{active, true},
			                   {backlog, 30},
			                   {certfile, "server.crt"},
			                   {depth, 0},
			                   {keepalive, true},
			                   {keyfile, "server.key"},
			                   {packet, 2},
			                   {reuse_sessions, false},
			                   {reuseaddr, true},
			                   {ssl_imp, new}],
			             ssl_listen_options([{active, true},{packet,2}])),
			?assertMatch([list,{active, false},
			                   {backlog, 30},
			                   {certfile, "../server.crt"},
			                   {depth, 0},
			                   {keepalive, true},
			                   {keyfile, "../server.key"},
			                   {packet, line},
			                   {reuse_sessions, false},
			                   {reuseaddr, true},
			                   {ssl_imp, new}],
			             ssl_listen_options([{certfile, "../server.crt"}, {keyfile, "../server.key"}]))
		end
		},
		{"ssl_connect_options merges provided proplist",
		fun() ->
			?assertMatch([list,{active, true},
			                   {depth, 0},
			                   {packet, 2},
			                   {ssl_imp, new}],
			             ssl_connect_options([{active, true},{packet,2}]))
		end
		}
	].

ssl_upgrade_test_() ->
	[
		{"TCP connection can be upgraded to ssl",
		fun() ->
			Self = self(),
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			spawn(fun() ->
			      	{ok, ListenSocket} = listen(tcp, ?TEST_PORT),
			      	{ok, ServerSocket} = accept(ListenSocket),
							{ok, NewServerSocket} = socket:to_ssl_server(ServerSocket, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
			      	Self ! NewServerSocket
			      end),
			{ok, ClientSocket} = connect(tcp, "localhost", ?TEST_PORT),
			?assert(is_port(ClientSocket)),
			{ok, NewClientSocket} = to_ssl_client(ClientSocket),
			?assertMatch([sslsocket|_], tuple_to_list(NewClientSocket)),
			receive NewServerSocket -> ok end,
			?assertMatch({sslsocket, _, _}, NewServerSocket),
			close(NewClientSocket),
			close(NewServerSocket)
		end
		},
		{"SSL server connection can't be upgraded again",
		fun() ->
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			spawn(fun() ->
						{ok, ListenSocket} = listen(ssl, ?TEST_PORT, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
						{ok, ServerSocket} = accept(ListenSocket),
						?assertException(error, ssl_connected, to_ssl_server(ServerSocket)),
						close(ServerSocket)
				end),
			{ok, ClientSocket} = connect(tcp, "localhost", ?TEST_PORT),
			close(ClientSocket)
		end
		},
		{"SSL client connection can't be upgraded again",
		fun() ->
			Self = self(),
			application:start(crypto),
			application:start(public_key),
			application:start(ssl),
			spawn(fun() ->
						{ok, ListenSocket} = listen(ssl, ?TEST_PORT, [{keyfile, "../testdata/server.key"}, {certfile, "../testdata/server.crt"}]),
						{ok, ServerSocket} = accept(ListenSocket),
						Self ! ServerSocket
				end),
			{ok, ClientSocket} = connect(ssl, "localhost", ?TEST_PORT),
			receive ServerSocket -> ok end,
			?assertException(error, ssl_connected, to_ssl_client(ClientSocket)),
			close(ClientSocket),
			close(ServerSocket)
		end
		}
	].
-endif.
