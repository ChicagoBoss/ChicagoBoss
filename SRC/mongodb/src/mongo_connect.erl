% Thread-safe TCP connection to a MongoDB server with synchronous call and asynchronous send interface.
-module (mongo_connect).

-export_type ([host/0, connection/0, dbconnection/0, failure/0]).

-export([connect/1, close/1]). % API
-export ([call/3, send/2]). % for mongo_query and mongo_cursor

-include_lib ("bson/include/bson_binary.hrl").

-type host() :: {inet:hostname() | inet:ip_address(), 0..65535}.
% Address and port where address may be ip address or its domain name
-type reason() :: any().

-opaque connection() :: mvar:mvar (gen_tcp:socket()).
% Thread-safe, TCP connection to a MongoDB server.
% Passive raw binary socket.

-spec connect (host()) -> {ok, connection()} | {error, reason()}. % IO
% Create connection to given MongoDB server or return reason for connection failure.
connect ({Addr, Port}) ->
	Init = fun () -> case gen_tcp:connect (Addr, Port, [binary, {active, false}, {packet, 0}]) of
		{ok, Socket} -> Socket;
		{error, Reason} -> throw (Reason) end end,
	try mvar:create (Init, fun gen_tcp:close/1)
		of Var -> {ok, Var}
		catch Reason -> {error, Reason} end.

-spec close (connection()) -> ok. % IO
% Close connection.
close (Conn) -> mvar:terminate (Conn).

-type dbconnection() :: {mongo_protocol:db(), connection()}.

-type failure() :: {connection_failure, connection(), reason()}.

-spec call (dbconnection(), [mongo_protocol:notice()], mongo_protocol:request()) -> mongo_protocol:reply(). % IO throws failure()
% Synchronous send and reply. Notices are sent right before request in single block. Exclusive access to connection during entire call.
call ({Db, Conn}, Notices, Request) ->
	{MessagesBin, RequestId} = messages_binary (Db, Notices ++ [Request]),
	Call = fun (Socket) ->
		tcp_send (Socket, MessagesBin),
		<<?get_int32 (N)>> = tcp_recv (Socket, 4),
		tcp_recv (Socket, N-4) end,
	try mvar:with (Conn, Call)
		of ReplyBin ->
			{RequestId, Reply, <<>>} = mongo_protocol:get_reply (ReplyBin),
			Reply  % ^ ResponseTo must match RequestId
		catch Reason -> throw ({connection_failure, Conn, Reason}) end.

-spec send (dbconnection(), [mongo_protocol:notice()]) -> ok. % IO throws failure()
% Asynchronous send (no reply). Don't know if send succeeded. Exclusive access to the connection during send.
send ({Db, Conn}, Notices) ->
	{NoticesBin, _} = messages_binary (Db, Notices),
	Send = fun (Socket) -> tcp_send (Socket, NoticesBin) end,
	try mvar:with (Conn, Send)
		catch Reason -> throw ({connection_failure, Conn, Reason}) end.

-spec messages_binary (mongo_protocol:db(), [mongo_protocol:message()]) -> {binary(), mongo_protocol:requestid()}.
% Binary representation of messages
messages_binary (Db, Messages) ->
	Build = fun (Message, {Bin, _}) -> 
		RequestId = mongodb_app:next_requestid(),
		MBin = mongo_protocol:put_message (Db, Message, RequestId),
		{<<Bin /binary, ?put_int32 (byte_size (MBin) + 4), MBin /binary>>, RequestId} end,
	lists:foldl (Build, {<<>>, 0}, Messages).

% Util %

tcp_send (Socket, Binary) -> case gen_tcp:send (Socket, Binary) of
	ok -> ok;
	{error, Reason} -> throw (Reason) end.

tcp_recv (Socket, N) -> case gen_tcp:recv (Socket, N) of
	{ok, Binary} -> Binary;
	{error, Reason} -> throw (Reason) end.
