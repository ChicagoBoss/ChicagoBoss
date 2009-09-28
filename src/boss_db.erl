%% @doc Chicago Boss database abstraction

-module(boss_db).

-export([start/0, start/1, stop/0]).

-export([find/1, find/3, find/4, find/5, find/6]).

-export([counter/1, incr/1, incr/2, delete/1, save_record/1, type/1]).

start() ->
    start([]).

start(_Options) ->
    application:start(boss_db).

stop() ->
    application:stop(boss_db).

%% @spec find(Id) -> {error, Reason} | BossRecord
%% @doc Find a BossRecord with the specified Id.
find(Key) ->
    gen_server:call(boss_db, {find, Key}).

%% @spec find(Type::atom(), Conditions, Max::integer()) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%% @doc Query for BossRecords. Returns up to Max number of BossRecords of type
%% Type matching all of the given Conditions (attribute = value).
find(Type, Conditions, Max) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max}).

%% @spec find( Type::atom(), Conditions, Max::integer(), Skip::integer() ) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%% @doc Query for BossRecords. Returns up to Max number of BossRecords of type
%% Type matching all of the given Conditions (attribute = value), skipping the
%% first Skip results.
find(Type, Conditions, Max, Skip) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max, Skip}).

%% @spec find( Type::atom(), Conditions, Max::integer(), Skip::integer(), Sort::atom() ) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%% @doc Query for BossRecords. Returns up to Max number of BossRecords of type
%% Type matching all of the given Conditions (attribute = value), skipping the
%% first Skip results, sorted on the attribute Sort.
find(Type, Conditions, Max, Skip, Sort) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max, Skip, Sort}).

%% @spec find( Type::atom(), Conditions, Max::integer(), Skip::integer(), Sort::atom(), SortOrder ) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%%       SortOrder = num_ascending | num_descending | str_ascending | str_descending
%% @doc Query for BossRecords. Returns up to Max number of BossRecords of type
%% Type matching all of the given Conditions (attribute = value), skipping the
%% first Skip results, sorted on the attribute Sort. SortOrder specifies whether
%% to treat values as strings or as numbers, and whether to sort ascending or
%% descending.
%%
%% Note that Time attributes are stored internally as numbers, so you should sort them numerically.

find(Type, Conditions, Max, Skip, Sort, SortOrder) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max, Skip, Sort, SortOrder}).

%% @spec counter( Id::string() ) -> integer()
%% @doc Treat the record associated with Id as a counter and return its value.
%% Returns 0 if the record does not exist, so to reset a counter just use
%% "delete".
counter(Key) ->
    gen_server:call(boss_db, {counter, Key}).

%% @spec incr( Id::string() ) -> integer()
%% @doc Treat the record associated with Id as a counter and atomically increment its value by 1.
incr(Key) ->
    gen_server:call(boss_db, {incr, Key}).

%% @spec incr( Id::string(), Increment::integer() ) -> integer()
%% @doc Treat the record associated with Id as a counter and atomically increment its value by Increment.
incr(Key, Count) ->
    gen_server:call(boss_db, {incr, Key, Count}).

%% @spec delete( Id::string() ) -> ok | {error, Reason}
%% @doc Delete the BossRecord with the given Id.
delete(Key) ->
    gen_server:call(boss_db, {delete, Key}).

%% @spec save_record( BossRecord ) -> SavedBossRecord
%% @doc Save (that is, create or update) the given BossRecord in the database.
save_record(Record) ->
    gen_server:call(boss_db, {save_record, Record}).

%% @spec type( Id::string() ) -> Type::atom()
%% @doc Returns the type of the BossRecord with Id, or undefined if the record does not exist.
type(Key) ->
    case find(Key) of
        {error, _} -> undefined;
        Record -> element(1, Record)
    end.
