% Top-level client interface to MongoDB
-module (mongo).

-export_type ([maybe/1]).

-export_type ([host/0, connection/0]).
-export ([connect/1, disconnect/1]).

-export_type ([action/1, db/0, write_mode/0, read_mode/0, failure/0]).
-export ([do/5]).

-export_type ([collection/0, selector/0, projector/0, skip/0, batchsize/0, modifier/0]).
-export ([insert/2, insert_all/2]).
-export ([save/2, replace/3, repsert/3, modify/3]).
-export ([delete/2, delete_one/2]).
-export ([find_one/2, find_one/3, find_one/4]).
-export ([find/2, find/3, find/4, find/5]).
-export ([count/2, count/3]).

-export_type ([cursor/0]).
-export ([next/1, rest/1, close_cursor/1]).

-export_type ([command/0]).
-export ([command/1]).

-export_type ([key_order/0, index_uniqueness/0]).
-export ([create_index/2, create_index/3, create_index/4]).

-include ("mongo_protocol.hrl").

-type reason() :: any().

-type host() :: inet:hostname() | inet:ip_address() | mongo_connect:host().
% Hostname or ip address with or without port. Port defaults to 27017 if port missing.
% Eg. "localhost" or {"localhost", 27017}

-type connection() :: mongo_connect:connection().

-spec connect (host()) -> {ok, connection()} | {error, reason()}. % IO
% Connect to given MongoDB server
connect ({Address, Port}) -> mongo_connect:connect ({Address, Port});
connect (Address) -> mongo_connect:connect ({Address, 27017}).

-spec disconnect (connection()) -> ok. % IO
% Close connection to server
disconnect (Conn) -> mongo_connect:close (Conn).

-type action(A) :: fun (() -> A).
% An Action does IO, reads process dict {mongo_action_context, #context{}}, and throws failure()

-type failure() ::
	mongo_connect:failure() |  % thrown by read and safe write
	mongo_query:not_master() |  % throws by read and safe write
	write_failure() |  % throws by safe write
	mongo_cursor:expired().  % thrown by cursor next/rest

-record (context, {
	write_mode :: write_mode(),
	read_mode :: read_mode(),
	dbconn :: mongo_connect:dbconnection() }).

-spec do (write_mode(), read_mode(), connection(), db(), action(A)) -> {ok, A} | {failure, failure()}. % IO
% Execute mongo action under given write_mode, read_mode, connection, and db. Return action result or failure.
do (WriteMode, ReadMode, Connection, Database, Action) ->
	PrevContext = get (mongo_action_context),
	put (mongo_action_context, #context {write_mode = WriteMode, read_mode = ReadMode, dbconn = {Database, Connection}}),
	try Action() of
		Result -> {ok, Result}
	catch
		throw: E = {connection_failure, _, _} -> {failure, E};
		throw: E = not_master -> {failure, E};
		throw: E = {write_failure, _, _} -> {failure, E};
		throw: E = {cursor_expired, _} -> {failure, E}
	after
		case PrevContext of undefined -> erase (mongo_action_context); _ -> put (mongo_action_context, PrevContext) end
	end.

% Write %

-type write_mode() :: unsafe | safe | {safe, mongo_query:getlasterror_request()}.
% Every write inside an action() will use this write mode.
% unsafe = asynchronous write (no reply) and hence may silently fail;
% safe = synchronous write, wait for reply and fail if connection or write failure;
% {safe, Params} = same as safe but with extra params for getlasterror, see its documentation.

-type write_failure() :: {write_failure, error_code(), bson:utf8()}.
-type error_code() :: integer().

-spec write (mongo_query:write()) -> ok. % Action
% Do unsafe unacknowledged fast write or safe acknowledged slower write depending on our context. When safe, throw write_failure if acknowledgment (getlasterror) reports error.
write (Write) ->
	Context = get (mongo_action_context),
	case Context #context.write_mode of
		unsafe -> mongo_query:write (Context #context.dbconn, Write);
		SafeMode -> 
			Params = case SafeMode of safe -> {}; {safe, Param} -> Param end,
			Ack = mongo_query:write (Context #context.dbconn, Write, Params),
			case bson:lookup (err, Ack) of
				{} -> ok; {null} -> ok;
				{String} -> case bson:at (code, Ack) of
					10058 -> throw (not_master);
					Code -> throw ({write_failure, Code, String}) end end end.

-spec insert (collection(), bson:document()) -> bson:value(). % Action
% Insert document into collection. Return its '_id' value, which is auto-generated if missing.
insert (Coll, Doc) -> [Value] = insert_all (Coll, [Doc]), Value.

-spec insert_all (collection(), [bson:document()]) -> [bson:value()]. % Action
% Insert documents into collection. Return their '_id' values, which are auto-generated if missing.
insert_all (Coll, Docs) ->
	Docs1 = lists:map (fun assign_id/1, Docs),
	write (#insert {collection = Coll, documents = Docs1}),
	lists:map (fun (Doc) -> bson:at ('_id', Doc) end, Docs1).

-spec assign_id (bson:document()) -> bson:document(). % IO
% If doc has no '_id' field then generate a fresh object id for it
assign_id (Doc) -> case bson:lookup ('_id', Doc) of
	{_Value} -> Doc;
	{} -> bson:append ({'_id', mongodb_app:gen_objectid()}, Doc) end.

-spec save (collection(), bson:document()) -> ok. % Action
% If document has no '_id' field then insert it, otherwise update it and insert only if missing.
save (Coll, Doc) -> case bson:lookup ('_id', Doc) of
	{} -> insert (Coll, Doc);
	{Id} -> repsert (Coll, {'_id', Id}, Doc) end.

-spec replace (collection(), selector(), bson:document()) -> ok. % Action
% Replace first document selected with given document.
replace (Coll, Selector, Doc) -> update (false, false, Coll, Selector, Doc).

-spec repsert (collection(), selector(), bson:document()) -> ok. % Action
% Replace first document selected with given document, or insert it if selection is empty.
repsert (Coll, Selector, Doc) -> update (true, false, Coll, Selector, Doc).

-spec modify (collection(), selector(), modifier()) -> ok. % Action
% Update all documents selected using modifier
modify (Coll, Selector, Mod) -> update (false, true, Coll, Selector, Mod).

-spec update (boolean(), boolean(), collection(), selector(), bson:document()) -> ok. % Action
update (Upsert, MultiUpdate, Coll, Sel, Doc) ->
	write (#update {collection = Coll, upsert = Upsert, multiupdate = MultiUpdate, selector = Sel, updater = Doc}).

-spec delete (collection(), selector()) -> ok. % Action
% Delete selected documents
delete (Coll, Selector) ->
	write (#delete {collection = Coll, singleremove = false, selector = Selector}).

-spec delete_one (collection(), selector()) -> ok. % Action
% Delete first selected document.
delete_one (Coll, Selector) ->
	write (#delete {collection = Coll, singleremove = true, selector = Selector}).

% Read %

-type read_mode() :: master | slave_ok.
% Every query inside an action() will use this mode.
% master = Server must be master/primary so reads are consistent (read latest writes).
% slave_ok = Server may be slave/secondary so reads are may not be consistent (may read stale data). But the slaves will eventually get the latest writes, so technically this is called eventually-consistent.

slave_ok (#context {read_mode = slave_ok}) -> true;
slave_ok (#context {read_mode = master}) -> false.

-type maybe(A) :: {A} | {}.

-spec find_one (collection(), selector()) -> maybe (bson:document()). % Action
% Return first selected document, if any
find_one (Coll, Selector) -> find_one (Coll, Selector, []).

-spec find_one (collection(), selector(), projector()) -> maybe (bson:document()). % Action
% Return projection of first selected document, if any. Empty projection [] means full projection.
find_one (Coll, Selector, Projector) -> find_one (Coll, Selector, Projector, 0).

-spec find_one (collection(), selector(), projector(), skip()) -> maybe (bson:document()). % Action
% Return projection of Nth selected document, if any. Empty projection [] means full projection.
find_one (Coll, Selector, Projector, Skip) ->
	Context = get (mongo_action_context),
	Query = #'query' {
		collection = Coll, selector = Selector, projector = Projector,
		skip = Skip, slaveok = slave_ok (Context) },
	mongo_query:find_one (Context #context.dbconn, Query).

-spec find (collection(), selector()) -> cursor(). % Action
% Return selected documents.
find (Coll, Selector) -> find (Coll, Selector, []).

-spec find (collection(), selector(), projector()) -> cursor(). % Action
% Return projection of selected documents. Empty projection [] means full projection.
find (Coll, Selector, Projector) -> find (Coll, Selector, Projector, 0).

-spec find (collection(), selector(), projector(), skip()) -> cursor(). % Action
% Return projection of selected documents starting from Nth document. Empty projection means full projection.
find (Coll, Selector, Projector, Skip) -> find (Coll, Selector, Projector, Skip, 0).

-spec find (collection(), selector(), projector(), skip(), batchsize()) -> cursor(). % Action
% Return projection of selected documents starting from Nth document in batches of batchsize. 0 batchsize means default batch size. Negative batch size means one batch only. Empty projection means full projection.
find (Coll, Selector, Projector, Skip, BatchSize) ->
	Context = get (mongo_action_context),
	Query = #'query' {
		collection = Coll, selector = Selector, projector = Projector,
		skip = Skip, batchsize = BatchSize, slaveok = slave_ok (Context) },
	mongo_query:find (Context #context.dbconn, Query).

-type cursor() :: mongo_cursor:cursor().

-spec next (cursor()) -> maybe (bson:document()). % IO throws mongo_connect:failure() & mongo_cursor:expired() (this is a subtype of Action)
% Return next document in query result cursor, if any.
next (Cursor) -> mongo_cursor:next (Cursor).

-spec rest (cursor()) -> [bson:document()]. % IO throws mongo_connect:failure() & mongo_cursor:expired() (this is a subtype of Action)
% Return remaining documents in query result cursor.
rest (Cursor) -> mongo_cursor:rest (Cursor).

-spec close_cursor (cursor()) -> ok. % IO (IO is a subtype of Action)
% Close cursor
close_cursor (Cursor) -> mongo_cursor:close (Cursor).

-spec count (collection(), selector()) -> integer(). % Action
% Count selected documents
count (Coll, Selector) -> count (Coll, Selector, 0).

-spec count (collection(), selector(), integer()) -> integer(). % Action
% Count selected documents up to given max number; 0 means no max. Ie. stops counting when max is reached to save processing time.
count (Coll, Selector, Limit) ->
	CollStr = atom_to_binary (Coll, utf8),
	Command = if
		Limit =< 0 -> {count, CollStr, 'query', Selector};
		true -> {count, CollStr, 'query', Selector, limit, Limit} end,
	Doc = command (Command),
	trunc (bson:at (n, Doc)). % Server returns count as float

% Command %

-type command() :: mongo_query:command().

-spec command (command()) -> bson:document(). % Action
% Execute given MongoDB command and return its result.
command (Command) ->
	Context = get (mongo_action_context),
	mongo_query:command (Context #context.dbconn, Command, slave_ok (Context)).

% Administration %

-type key_order() :: bson:document().
% List keys and whether ascending (1) or descending (-1). Eg. {x,1, y,-1}

-spec create_index (collection(), key_order()) -> ok. % Action
% Create non-unique index on given keys in collection
create_index (Coll, KeyOrder) ->
	create_index (Coll, KeyOrder, non_unique).

-type index_uniqueness() ::
	non_unique |  % Multiple docs with same index value allowed
	unique |  % At most one doc with same index value, index creation fails otherwise
	unique_dropdups.  % Same as unique, but deletes docs with duplicate index value on index creation

-spec create_index (collection(), key_order(), index_uniqueness()) -> ok. % Action
% Create index on given keys with given uniqueness
create_index (Coll, KeyOrder, Uniqueness) ->
	create_index (Coll, KeyOrder, Uniqueness, gen_index_name (KeyOrder)).

-spec gen_index_name (key_order()) -> bson:utf8().
gen_index_name (KeyOrder) ->
	AsName = fun (Label, Order, Name) -> <<
		$_,
		Name /binary,
		(atom_to_binary (Label, utf8)) /binary,
		$_,
		(bson:utf8 (integer_to_list (Order))) /binary >> end,
	bson:doc_foldl (AsName, <<>>, KeyOrder).

-spec create_index (collection(), key_order(), index_uniqueness(), bson:utf8()) -> ok. % Action
% Create index on given keys with given uniqueness and name
create_index (Coll, KeyOrder, Uniqueness, IndexName) ->
	{Db, _} = (get (mongo_action_context)) #context.dbconn,
	{Unique, DropDups} = case Uniqueness of
		non_unique -> {false, false};
		unique -> {true, false};
		unique_dropdups -> {true, true} end,
	insert ('system.indexes', {
		ns, mongo_protocol:dbcoll (Db, Coll),
		key, KeyOrder,
		name, IndexName,
		unique, Unique,
		dropDups, DropDups}).
