The Database README
===================

By default Chicago Boss uses an in-memory database, which is useful for
development and testing but not much else.


Using PostgreSQL/MySQL
----------------------

Open up boss.config and set db_adapter to `pgsql` or `mysql`. You should also set:

 - db_host
 - db_port
 - db_username
 - db_password
 - db_database

Set the db_port appropriately. By default, PostgreSQL listens on port 5432.

Models
- - -

To use Chicago Boss with a SQL database, you need to create a table for each of
your model files. The table name should be the plural of the model name. Field
names should be the underscored versions of the model attribute names. Use
whatever data types make sense for your application.

ID fields
- - - - -

The "id" field should be a serial integer. (However, the generated 
BossRecord ID exposed to your application will still have the form "model_name-1".)

Here are useful starting points for MySQL and PostgreSQL respectively:

    CREATE TABLE models (
        id MEDIUMINT NOT NULL AUTO_INCREMENT,
        ...
        PRIMARY KEY (id)
    );

    CREATE TABLE models (
        id SERIAL PRIMARY KEY,
        ...
    );

Counters
- - - -

To use the counter features, you need a table called "counters" with a "name"
and "value" column, like these: 

    CREATE TABLE counters (
        name                VARCHAR(255) PRIMARY KEY,
        value               INTEGER DEFAULT 0
    );


Using Tokyo Tyrant
------------------

Tokyo Tyrant is a non-relational database developed by FAL Labs.

1. Download and install Tokyo Tyrant from <http://fallabs.com/tokyotyrant/>

2. Run Tyrant with a table database. Other database types are not supported.

3. Open up boss.config and set db_adapter to tyrant

Set the db_port appropriately. By default, Tokyo Tyrant listens on port 1978.


Using MongoDB
------------------

MongoDB is a document-oriented NoSQL database developed by 10gen.

1. Download and install MongoDB from <http://mongodb.org>

2. Start MongoDB with `mongod`

3. Open up boss.config and set the db_adapter to mongodb and db_database to the name of your choice. The database will be created automatically, with the name you specified.

Set the db_port appropriately. By default, MongoDB listens on port 27017.


Using Mnesia
------------

Notes:

1. The erlang VM needs a name and cookie to run Mnesia. You can set them with
   the 'vm_name' and 'vm_cookie' options in boss.config.

2. Create a table for each model as well a table called '_ids_' with attributes
   for [type, id]. For more details see

   https://github.com/evanmiller/ChicagoBoss/wiki/Automatic-schema-initialization-for-mnesia

3. Open up boss.config and set db_adapter to mnesia


Using Riak
-------------------------

Riak is a NoSQL database developed by Basho Technologies.

1. Install Riak as described at http://wiki.basho.com/Installation-and-Setup.html

2. Start a Riak node with `sudo riak start`

3. Open up boss.config and set db_adapter to riak.

For query operations to work, you first need to ensure that search is enabled
in your Riak configuration:

   {riak_search, [{enabled, true}]}
    
Then run the following command in your Riak installation for each model:

   bin/search-cmd install <plural model name>

E.g. if you have a "greeting" model the command should be
   
   bin/search-cmd install greetings

Database Migrations
-------------------

Chicago Boss supports Rails-like database migrations, which work like so:

    > boss_migrate:make(your_app_name, create_some_table).

This creates a file in priv/migrations/ with a name like 1363463613_create_some_table.erl

Edit this file in order to do something along these lines:

    {create_some_table,
      fun(up) ->
	      boss_db:execute("create table some_table ( id serial unique, name varchar,
	                       birth_date timestamp, created_at timestamp )");
	 (down) ->
	      boss_db:execute("drop table some_table")
      end}.

To actually run the migrations, you'd do

     > boss_migrate:run(gsd_web).

You can also redo the last migration - which is useful when you're
developing and perhaps haven't got the table definition quite right:

     > boss_migrate:redo(gsd_web, create_some_table).

Internally, migrations are tracked in a table called
"schema_migrations" which is automatically created and managed by
boss_db.
