%% @doc Chicago Boss database abstraction

-module(boss_db).

-export([start/0, start/1, stop/0]).

-export([
        find/1, 
        find/3, 
        find/4, 
        find/5, 
        find/6,
        count/1,
        count/2,
        counter/1, 
        incr/1, 
        incr/2, 
        delete/1, 
        save_record/1, 
        validate_record/1,
        type/1,
        data_type/2]).

start() ->
    start([]).

start(_Options) ->
    application:start(boss_db).

stop() ->
    application:stop(boss_db).

%% @spec find(Id::string()) -> BossRecord | {error, Reason}
%% @doc Find a BossRecord with the specified `Id'.
find(Key) ->
    gen_server:call(boss_db, {find, Key}).

%% @spec find(Type::atom(), Conditions, Max::integer()) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% `Type' matching all of the given `Conditions' (attribute = value).
find(Type, Conditions, Max) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max}).

%% @spec find( Type::atom(), Conditions, Max::integer(), Skip::integer() ) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% `Type' matching all of the given `Conditions' (attribute = value), skipping the
%% first `Skip' results.
find(Type, Conditions, Max, Skip) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max, Skip}).

%% @spec find( Type::atom(), Conditions, Max::integer(), Skip::integer(), Sort::atom() ) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% `Type' matching all of the given `Conditions' (attribute = value), skipping the
%% first `Skip' results, sorted on the attribute `Sort'.
find(Type, Conditions, Max, Skip, Sort) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max, Skip, Sort}).

%% @spec find( Type::atom(), Conditions, Max::integer(), Skip::integer(), Sort::atom(), SortOrder ) -> [ BossRecord ]
%%       Conditions = [{Key::atom(), Value::string()}]
%%       SortOrder = num_ascending | num_descending | str_ascending | str_descending
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% Type matching all of the given `Conditions' (attribute = value), skipping the
%% first `Skip' results, sorted on the attribute `Sort'. `SortOrder' specifies whether
%% to treat values as strings or as numbers, and whether to sort ascending or
%% descending. (`SortOrder' = `num_ascending', `num_descending', `str_ascending', or
%% `str_descending')
%%
%% Note that Time attributes are stored internally as numbers, so you should
%% sort them numerically.

find(Type, Conditions, Max, Skip, Sort, SortOrder) ->
    gen_server:call(boss_db, {find, Type, Conditions, Max, Skip, Sort, SortOrder}).

%% @spec count( Type::atom() ) -> integer()
%% @doc Count the number of BossRecords of type `Type' in the database.
count(Type) ->
    gen_server:call(boss_db, {count, Type}).

%% @spec count( Type::atom(), Conditions ) -> integer()
%%       Conditions = [{Key::atom(), Value::string()}]
%% @doc Count the number of BossRecords of type `Type' in the database matching
%% all of the given `Conditions' (attribute = value).
count(Type, Conditions) ->
    gen_server:call(boss_db, {count, Type, Conditions}).

%% @spec counter( Id::string() ) -> integer()
%% @doc Treat the record associated with `Id' as a counter and return its value.
%% Returns 0 if the record does not exist, so to reset a counter just use
%% "delete".
counter(Key) ->
    gen_server:call(boss_db, {counter, Key}).

%% @spec incr( Id::string() ) -> integer()
%% @doc Treat the record associated with `Id' as a counter and atomically increment its value by 1.
incr(Key) ->
    gen_server:call(boss_db, {incr, Key}).

%% @spec incr( Id::string(), Increment::integer() ) -> integer()
%% @doc Treat the record associated with `Id' as a counter and atomically increment its value by `Increment'.
incr(Key, Count) ->
    gen_server:call(boss_db, {incr, Key, Count}).

%% @spec delete( Id::string() ) -> ok | {error, Reason}
%% @doc Delete the BossRecord with the given `Id'.
delete(Key) ->
    gen_server:call(boss_db, {delete, Key}).

%% @spec save_record( BossRecord ) -> {ok, SavedBossRecord} | {error, [ErrorMessages]}
%% @doc Save (that is, create or update) the given BossRecord in the database.
%% Performs validation first; see `validate_record/0'.
save_record(Record) ->
    case validate_record(Record) of
        ok -> gen_server:call(boss_db, {save_record, Record});
        Err -> Err
    end.

%% @spec validate_record( BossRecord ) -> ok | {error, [ErrorMessages]}
%% @doc Validate the given BossRecord without saving it in the database.
%% `ErrorMessages' are generated from the list of tests returned by the BossRecord's 
%% `validation_tests/0' function (if defined). The returned list should consist of
%% `{TestFunction, ErrorMessage}' tuples, where `TestFunction' is a fun of arity 0 
%% that returns `true' if the record is valid or `false' if it is invalid. 
%% `ErrorMessage' should be a (constant) string which will be included in `ErrorMessages'
%% if the `TestFunction' returns `false' on this particular BossRecord.
validate_record(Record) ->
    Type = element(1, Record),
    Errors = case lists:member({validation_tests, 1}, Type:module_info(exports)) of
        true -> [String || {TestFun, String} <- Record:validation_tests(), not TestFun()];
        false -> []
    end,
    case length(Errors) of
        0 -> ok;
        _ -> {error, Errors}
    end.

%% @spec type( Id::string() ) -> Type::atom()
%% @doc Returns the type of the BossRecord with `Id', or `undefined' if the record does not exist.
type(Key) ->
    case find(Key) of
        undefined -> undefined;
        Record -> element(1, Record)
    end.

data_type(_, _Val) when is_float(_Val) ->
    "float";
data_type(_, _Val) when is_binary(_Val) ->
    "binary";
data_type(_, _Val) when is_integer(_Val) ->
    "integer";
data_type(_, _Val) when is_tuple(_Val) ->
    "datetime";
data_type('id', _) ->
    "id";
data_type(Key, Val) when is_list(Val) ->
    case lists:suffix("_id", atom_to_list(Key)) of
        true -> "foreign_id";
        false -> "string"
    end.
