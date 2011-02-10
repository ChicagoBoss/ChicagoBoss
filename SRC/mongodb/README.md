This is the MongoDB driver for Erlang. [MongoDB](http://www.mongodb.org) is a document-oriented database management system. A driver is a client library that provides an API for connecting to MongoDB servers, performing queries and updates on those servers, and performing administrative tasks like creating indexes and viewing statistics.

This first version of the driver only supports individual connections to single servers, although multiple processes can use the same connection simultaneously without interfering with each other. This version does not support connection pooling and smart replica-set connection, which will be included in the next version coming out in a few weeks.

This driver is implemented as an Erlang application named *mongodb*. It depends on another Erlang library application named [*bson*](http://github.com/TonyGen/bson-erlang), which defines the document type and its standard binary representation. You need both of these. Below we describe the mongodb application; you should also see the bson application to understand the document type.

Downloaded and compile each application

	$ git clone git://github.com/TonyGen/bson-erlang.git bson
	$ git clone git://github.com/TonyGen/mongodb-erlang.git mongodb
	$ cd bson
	$ erlc -o ebin -I include src/*.erl
	$ cd ../mongodb
	$ erlc -o ebin -I include -I .. src/*.erl
	$ cd ..

Then install them in your standard Erlang library location or include them in your path on startup

	$ erl -pa bson/ebin mongodb/ebin

The mongodb application needs be started before using (to initialize an internal ets table of counters)

	> application:start (mongodb).

Although the mongodb application includes several modules, you should only need to use *mongo*, which is the top-level interface for the driver. Likewise, you should only need to use the *bson* module in the bson application.

Start a mongodb server listening on `localhost:27017` (or any address & port of your choosing), then connect to it

	> {ok, Conn} = mongo:connect ({localhost, 27017}).

`27017` is the default port so you could elide it in this case and just supply `localhost` as the argument. `mongo:connect` returns `{error, Reason}` if it failed to connect.

A database operation happens in the context of a connection, database, read-mode, and write-mode. These four parameters are supplied once at the beginning of a sequence of read/write operations. Furthermore, if one of the operations fails no further operations in the sequence are executed and an error is returned for the entire sequence.

	> mongo:do (safe, master, Conn, test, fun() ->
		mongo:delete (foo, {}),
		mongo:insert (foo, {x,1, y,2}),
		mongo:find (foo, {x,1}) end).

`safe`, along with `{safe, GetLastErrorParams}` and `unsafe`, are write-modes. Safe mode makes a *getLastError* request after every write in the sequence. If the reply says it failed then the rest of the sequence is aborted and `mongo:do` returns `{failure, {write_failure, Reason}}`, or `{failure, not_master}` when connected to a slave. An example write failure is attempting to insert a duplicate key that is indexed to be unique. Alternatively, unsafe mode issues every write without a confirmation, so if a write fails you won't know about it and remaining operations will be executed. This is unsafe but faster because you there is no round-trip delay.

`master`, along with `slave_ok`, are read-modes. `master` means every query in the sequence must read fresh data (from a master/primary server). If the connected server is not a master then the first read will fail, the remaining operations will be aborted, and `mongo:do` will return `{failure, not_master}`. `slave_ok` means every query is allowed to read stale data from a slave (fresh data from a master is fine too).

`Conn` is the connection we send the operations to. If it fails during one of the operations then the remaining operations are aborted and `{failure, {connection_failure, Reason}}` is returned.

`test` is the name of the database in this example. Collections accessed in the sequence (`foo` in this example) are taken from the database given in the fourth argument. If a collection is missing from the database it will be automatically created upon first access.

If there are no errors in the sequence of operations then the result of the last operation is returned as the result of the entire `mongo:do` command. It is wrapped in an *ok* tuple as in `{ok, Result}` to distinguish it from an error.

`mongo:find` returns a *cursor* holding the pending list of results, which are accessed using `mongo:next` to get the next result, and `mongo:rest` to get the remaining results. Either one throws `{cursor_expired, Cursor}` if the cursor was idle for more than 10 minutes. This exception is caught by `mongo:do` and returned as `{failure, {cursor_expired, Cursor}}`. `mongo:rest` also closes the cursor, otherwise you should close the cursor when finished using `mongo:close_cursor`.

Finally, you should close the connection when finished using `mongo:disconnect`.

See the [*mongo* module](http://github.com/TonyGen/mongodb-erlang/blob/master/src/mongo.erl) for a description of all operations. A type specification is provided with each operation so you know the expected arguments and results. The spec line also has a comment if it performs a side-effect such as IO and what exceptions it may throw. No comment means it is a pure function. Also, see the [*bson* module](http://github.com/TonyGen/bson-erlang/blob/master/src/bson.erl) in the bson application for details on the document type and its value types.

This driver does not provide helper functions for commands. Use `mongo:command` directly and refer to the [MongoDB documentation](http://www.mongodb.org/display/DOCS/Commands) for how to issue raw commands. A future version will probably include helper functions for the most common commands.
