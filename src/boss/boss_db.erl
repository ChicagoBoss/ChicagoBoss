%% @doc Chicago Boss database abstraction

-module(boss_db).

-export([start/0, start/1, stop/0]).

-export([
        find/1, 
        find/2, 
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
        push/0,
        pop/0,
        depth/0,
        dump/0,
        execute/1,
        transaction/1,
        validate_record/1,
        type/1,
        data_type/2]).

-define(DEFAULT_MAX, (1000 * 1000 * 1000)).
-define(DEFAULT_TIMEOUT, (30 * 1000)).

start() ->
    DBOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [db_port, db_host, db_username, db_password, db_database]),
    DBAdapter = boss_env:get_env(db_adapter, mock),
    DBShards = boss_env:get_env(db_shards, []),
    DBOptions1 = [{adapter, list_to_atom("boss_db_adapter_"++atom_to_list(DBAdapter))},
        {shards, DBShards}|DBOptions],
    start(DBOptions1).

start(Options) ->
    boss_db_sup:start_link(Options).

stop() ->
    ok.

%% @spec find(Id::string()) -> BossRecord | {error, Reason}
%% @doc Find a BossRecord with the specified `Id'.
find("") -> undefined;
find(Key) when is_list(Key) ->
    gen_server:call(boss_db, {find, Key}, ?DEFAULT_TIMEOUT);
find(_) ->
    {error, invalid_id}.

%% @spec find(Type::atom(), Conditions) -> [ BossRecord ]
%% @doc Query for BossRecords. Returns all BossRecords of type
%% `Type' matching all of the given `Conditions'
find(Type, Conditions) ->
    find(Type, Conditions, ?DEFAULT_MAX).

%% @spec find(Type::atom(), Conditions, Max::integer() | all ) -> [ BossRecord ]
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% `Type' matching all of the given `Conditions'
find(Type, Conditions, Max) ->
    find(Type, Conditions, Max, 0).

%% @spec find( Type::atom(), Conditions, Max::integer() | all, Skip::integer() ) -> [ BossRecord ]
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% `Type' matching all of the given `Conditions', skipping the first `Skip' results.
find(Type, Conditions, Max, Skip) ->
    find(Type, Conditions, Max, Skip, id).

%% @spec find( Type::atom(), Conditions, Max::integer() | all, Skip::integer(), Sort::atom() ) -> [ BossRecord ]
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% `Type' matching all of the given `Conditions', skipping the
%% first `Skip' results, sorted on the attribute `Sort'.
find(Type, Conditions, Max, Skip, Sort) ->
    find(Type, Conditions, Max, Skip, Sort, str_ascending).

%% @spec find( Type::atom(), Conditions, Max::integer() | all, Skip::integer(), Sort::atom(), SortOrder ) -> [ BossRecord ]
%%       SortOrder = num_ascending | num_descending | str_ascending | str_descending
%% @doc Query for BossRecords. Returns up to `Max' number of BossRecords of type
%% Type matching all of the given `Conditions', skipping the
%% first `Skip' results, sorted on the attribute `Sort'. `SortOrder' specifies whether
%% to treat values as strings or as numbers, and whether to sort ascending or
%% descending. (`SortOrder' = `num_ascending', `num_descending', `str_ascending', or
%% `str_descending')
%%
%% Note that Time attributes are stored internally as numbers, so you should
%% sort them numerically.

find(Type, Conditions, many, Skip, Sort, SortOrder) ->
    find(Type, Conditions, ?DEFAULT_MAX, Skip, Sort, SortOrder);
find(Type, Conditions, all, Skip, Sort, SortOrder) ->
    find(Type, Conditions, ?DEFAULT_MAX, Skip, Sort, SortOrder);
find(Type, Conditions, Max, Skip, Sort, SortOrder) ->
    gen_server:call(boss_db, {find, Type, normalize_conditions(Conditions), Max, Skip, Sort, SortOrder},
    ?DEFAULT_TIMEOUT).

%% @spec count( Type::atom() ) -> integer()
%% @doc Count the number of BossRecords of type `Type' in the database.
count(Type) ->
    count(Type, []).

%% @spec count( Type::atom(), Conditions ) -> integer()
%% @doc Count the number of BossRecords of type `Type' in the database matching
%% all of the given `Conditions'.
count(Type, Conditions) ->
    gen_server:call(boss_db, {count, Type, normalize_conditions(Conditions)}, ?DEFAULT_TIMEOUT).

%% @spec counter( Id::string() ) -> integer()
%% @doc Treat the record associated with `Id' as a counter and return its value.
%% Returns 0 if the record does not exist, so to reset a counter just use
%% "delete".
counter(Key) ->
    gen_server:call(boss_db, {counter, Key}, ?DEFAULT_TIMEOUT).

%% @spec incr( Id::string() ) -> integer()
%% @doc Treat the record associated with `Id' as a counter and atomically increment its value by 1.
incr(Key) ->
    incr(Key, 1).

%% @spec incr( Id::string(), Increment::integer() ) -> integer()
%% @doc Treat the record associated with `Id' as a counter and atomically increment its value by `Increment'.
incr(Key, Count) ->
    gen_server:call(boss_db, {incr, Key, Count}, ?DEFAULT_TIMEOUT).

%% @spec delete( Id::string() ) -> ok | {error, Reason}
%% @doc Delete the BossRecord with the given `Id'.
delete(Key) ->
    AboutToDelete = boss_db:find(Key),
    boss_record_lib:run_before_delete_hooks(AboutToDelete),
    case gen_server:call(boss_db, {delete, Key}, ?DEFAULT_TIMEOUT) of
        ok -> 
            boss_news:deleted(Key, AboutToDelete:attributes()),
            ok;
        RetVal -> 
            RetVal
    end.

push() ->
    gen_server:call(boss_db, push, ?DEFAULT_TIMEOUT).

pop() ->
    gen_server:call(boss_db, pop, ?DEFAULT_TIMEOUT).

depth() ->
    gen_server:call(boss_db, depth, ?DEFAULT_TIMEOUT).

dump() ->
    gen_server:call(boss_db, depth, ?DEFAULT_TIMEOUT).

%% @spec execute( Commands::iolist() ) -> RetVal
%% @doc Execute raw database commands on SQL databases
execute(Commands) ->
    gen_server:call(boss_db, {execute, Commands}, ?DEFAULT_TIMEOUT).

%% @spec transaction( TransactionFun::function() ) -> {atomic, Result} | {aborted, Reason}
%% @doc Execute a fun inside a transaction.
transaction(TransactionFun) ->
    gen_server:call(boss_db, {transaction, TransactionFun}, ?DEFAULT_TIMEOUT).

%% @spec save_record( BossRecord ) -> {ok, SavedBossRecord} | {error, [ErrorMessages]}
%% @doc Save (that is, create or update) the given BossRecord in the database.
%% Performs validation first; see `validate_record/1'.
save_record(Record) ->
    case validate_record(Record) of
        ok ->
            RecordId = Record:id(),
            {IsNew, OldRecord} = if
                RecordId =:= 'id' ->
                    {true, Record};
                true ->
                    case find(RecordId) of
                        {error, _Reason} -> {true, Record};
                        undefined -> {true, Record};
                        FoundOldRecord -> {false, FoundOldRecord}
                    end
            end,
            boss_record_lib:run_before_hooks(Record, IsNew),
            case gen_server:call(boss_db, {save_record, Record}, ?DEFAULT_TIMEOUT) of
                {ok, SavedRecord} ->
                    boss_record_lib:run_after_hooks(OldRecord, SavedRecord, IsNew),
                    {ok, SavedRecord};
                Err -> Err
            end;
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
    Errors = case erlang:function_exported(Type, validation_tests, 1) of
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
data_type(_, _Val) when is_boolean(_Val) ->
    "boolean";
data_type(_, undefined) ->
    "null";
data_type('id', _) ->
    "id";
data_type(Key, Val) when is_list(Val) ->
    case lists:suffix("_id", atom_to_list(Key)) of
        true -> "foreign_id";
        false -> "string"
    end.

normalize_conditions(Conditions) ->
    normalize_conditions(Conditions, []).

normalize_conditions([], Acc) ->
    lists:reverse(Acc);
normalize_conditions([Key, Operator, Value|Rest], Acc) when is_atom(Key), is_atom(Operator) ->
    normalize_conditions(Rest, [{Key, Operator, Value}|Acc]);
normalize_conditions([{Key, Value}|Rest], Acc) when is_atom(Key) ->
    normalize_conditions(Rest, [{Key, 'equals', Value}|Acc]);
normalize_conditions([{Key, Operator, Value}|Rest], Acc) when is_atom(Key), is_atom(Operator) ->
    normalize_conditions(Rest, [{Key, Operator, Value}|Acc]).
