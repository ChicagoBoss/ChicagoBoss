-module(boss_db_adapter_mongodb).
-behaviour(boss_db_adapter).
-export([start/0, start/1, stop/1, find/2, find/7]).
-export([count/3, counter/2, incr/3, delete/2, save_record/2]).
-export([push/2, pop/2, dump/1, execute/2]).
-define(GREGORIAN_SECONDS_1970, 62167219200). % http://stackoverflow.com/questions/3672287/date-to-megaseconds
-define(LOG(Name, Value), io:format("@@@ ~s: ~p~n", [Name, Value])).

start() ->
    start([]).

start(Options) ->
    Host = proplists:get_value(db_host, Options, "localhost"),
    Port = proplists:get_value(db_port, Options, 27017),
    Database = proplists:get_value(db_database, Options, test),
    WriteMode = proplists:get_value(db_write_mode, Options, safe),
    ReadMode = proplists:get_value(db_read_mode, Options, master),
    application:start(mongodb),
    {ok, Connection} = mongo:connect({Host, Port}),
    % We pass around arguments required by mongo:do/5
    {ok, {WriteMode, ReadMode, Connection, Database}}.

stop({_, _, Connection, _}) ->
    mongo:disconnect(Connection).

execute({WriteMode, ReadMode, Connection, Database}, Fun) ->
    mongo:do(WriteMode, ReadMode, Connection, Database, Fun).


find(Conn, Id) when is_list(Id) ->
    {Type, Collection, MongoId} = infer_type_from_id(Id),
    
    Res = execute(Conn, fun() ->
                mongo:find_one(Collection, {'_id', MongoId})
        end),

    case Res of
        {ok, {Doc}} -> mongo_tuple_to_record(Type, Doc);
        {failure, Reason} -> {error, Reason};
        {connection_failure, Reason} -> {error, Reason}
    end.


tuple_to_proplist(Tuple) ->
    List = tuple_to_list(Tuple),
    Ret = lists:reverse(list_to_proplist(List, [])),
    Ret.

list_to_proplist([], Acc) -> Acc;
list_to_proplist([K,V|T], Acc) ->
    list_to_proplist(T, [{K, V}|Acc]).

mongo_to_boss_id(Type, MongoId) ->
    lists:concat([Type, "-", binary_to_list(dec2hex(element(1, MongoId)))]).

boss_to_mongo_id(BossId) ->
    [_, MongoId] = string:tokens(BossId, "-"),
    {hex2dec(MongoId)}.

mongo_tuple_to_record(Type, Row) ->
    PropList = tuple_to_proplist(Row),
    Args = lists:map(fun({K,V}) ->
                case {K,V} of
                    {'_id', V} -> mongo_to_boss_id(Type, V);
                    {K, {_,_,_} = V} -> 
                        case lists:suffix("_time", atom_to_list(K)) of
                            true -> calendar:now_to_datetime(V);
                            false -> V
                        end;
                    {_, V} -> V
                end
        end, PropList),
    apply(Type, new, Args).



find(Conn, Type, Conditions, Max, Skip, Sort, SortOrder) when is_atom(Type), is_list(Conditions), 
                                                              is_integer(Max), is_integer(Skip), 
                                                              is_atom(Sort), is_atom(SortOrder) ->
    case model_is_loaded(Type) of
        true ->
            Collection = type_to_collection(Type),
            Res = execute(Conn, fun() ->
                    mongo:find(Collection, proplist_to_tuple(Conditions), [],
                               Skip, Max) 
                end),
            case Res of
                {ok, Curs} ->
                    RecordArgs = 
                        lists:map(fun(Row) ->
                                    mongo_tuple_to_record(Type, Row)
                            end, mongo:rest(Curs));
                {failure, Reason} -> {error, Reason};
                {connection_failure, Reason} -> {error, Reason}
            end;
        false -> {error, {module_not_loaded, Type}}
    end.


count(Conn, Type, Conditions) ->
    Collection = type_to_collection(Type),
    {ok, Count} = execute(Conn, fun() -> 
                mongo:count(Collection, proplist_to_tuple(Conditions))
        end),
    Count.

counter(Conn, Id) when is_list(Id) ->
    Res = pgsql:equery(Conn, "SELECT value FROM counters WHERE name = $1", [Id]),
    case Res of
        {ok, _, [{Value}]} -> Value;
        {error, _Reason} -> 0
    end.

incr(Conn, Id, Count) ->
    Res = pgsql:equery(Conn, "UPDATE counters SET value = value + $1 WHERE name = $2 RETURNING value", 
        [Count, Id]),
    case Res of
        {ok, _, _, [{Value}]} -> Value;
        {error, _Reason} -> 
            Res1 = pgsql:equery(Conn, "INSERT INTO counters (name, value) VALUES ($1, $2) RETURNING value", 
                [Id, Count]),
            case Res1 of
                {ok, _, _, [{Value}]} -> Value;
                {error, Reason} -> {error, Reason}
            end
    end.

delete(Conn, Id) when is_list(Id) ->
    {_Type, Collection, MongoId} = infer_type_from_id(Id),
    
    Res = execute(Conn, fun() ->
                mongo:delete(Collection, {'_id', MongoId})
        end),
    case Res of
        {ok, ok} -> ok;
        {failure, Reason} -> {error, Reason};
        {connection_failure, Reason} -> {error, Reason}
    end.

proplist_to_tuple(PropList) ->
    ListOfLists = lists:reverse([[K,V]||{K,V} <- PropList]),
    list_to_tuple(lists:foldl(
            fun([K, V], Acc) ->
                    [K,V|Acc] 
            end, [], ListOfLists)).

type_to_collection(Type) ->
    list_to_atom(type_to_table_name(Type)).

save_record(Conn, Record) when is_tuple(Record) ->
    Type = element(1, Record),
    Collection = type_to_collection(Type),
    Attributes = case Record:id() of
        id ->
            PropList = lists:map(fun({K,V}) -> {K, pack_value(V)} end, 
                                 Record:attributes()),
            Tuple = proplist_to_tuple(proplists:delete(id, PropList)),
            Tuple;
        DefinedId when is_list(DefinedId) ->
            PropList = lists:map(fun({K,V}) ->
                            case K of 
                                id -> {'_id', boss_to_mongo_id(DefinedId)};
                                _ -> {K, pack_value(V)}
                            end
                    end, Record:attributes()),
            proplist_to_tuple(PropList)
    end,
    Res = execute(Conn, fun() -> 
                mongo:save(Collection, Attributes)
        end),
    case Res of
        {ok, ok} -> {ok, Record};
        {ok, Id} -> 
            {ok, Record:id(mongo_to_boss_id(Type, Id))};
        {failure, Reason} -> {error, Reason};
        {connection_failure, Reason} -> {error, Reason}
    end.

push(Conn, Depth) ->
    ok.

pop(Conn, Depth) ->
    ok.

dump(_Conn) -> "".

%execute(Conn, Commands) ->
%    pgsql:squery(Conn, Commands).

% internal

infer_type_from_id(Id) when is_list(Id) ->
    [Type, BossId] = string:tokens(Id, "-"),
    {list_to_atom(Type), type_to_collection(Type), boss_to_mongo_id(Id)}.

type_to_table_name(Type) when is_atom(Type) ->
    type_to_table_name(atom_to_list(Type));
type_to_table_name(Type) when is_list(Type) ->
    inflector:pluralize(Type).

activate_record(Record, Metadata, Type) ->
    DummyRecord = apply(Type, new, lists:seq(1, proplists:get_value(new, Type:module_info(exports)))),
    apply(Type, new, lists:map(fun
                (id) ->
                    Index = keyindex(<<"id">>, 2, Metadata),
                    atom_to_list(Type) ++ "-" ++ integer_to_list(element(Index, Record));
                (Key) ->
                    Index = keyindex(list_to_binary(atom_to_list(Key)), 2, Metadata),
                    case element(Index, Record) of
                        {Date, {_, _, S} = Time} when is_float(S) ->
                            {Date, setelement(3, Time, round(S))};
                        Val -> Val
                    end
            end, DummyRecord:attribute_names())).

model_is_loaded(Type) ->
    case code:is_loaded(Type) of
        {file, _Loaded} ->
            Exports = Type:module_info(exports),
            case proplists:get_value(attribute_names, Exports) of
                1 -> true;
                _ -> false
            end;
        false -> false
    end.

keyindex(Key, N, TupleList) ->
    keyindex(Key, N, TupleList, 1).

keyindex(_Key, _N, [], _Index) ->
    undefined;
keyindex(Key, N, [Tuple|Rest], Index) ->
    case element(N, Tuple) of
        Key -> Index;
        _ -> keyindex(Key, N, Rest, Index + 1)
    end.

sort_order_sql(num_ascending) ->
    "ASC";
sort_order_sql(num_descending) ->
    "DESC";
sort_order_sql(str_ascending) ->
    "ASC";
sort_order_sql(str_descending) ->
    "DESC".

build_insert_query(Record) ->
    Type = element(1, Record),
    TableName = type_to_table_name(Type),
    {Attributes, Values} = lists:foldl(fun
            ({id, _}, Acc) -> Acc;
            ({A, V}, {Attrs, Vals}) ->
                {[atom_to_list(A)|Attrs], [pack_value(V)|Vals]}
        end, {[], []}, Record:attributes()),
    ["INSERT INTO ", TableName, " (", 
        string:join(Attributes, ", "),
        ") values (",
        string:join(Values, ", "),
        ")",
        " RETURNING id"
    ].

build_update_query(Record) ->
    {_, TableName, Id} = infer_type_from_id(Record:id()),
    Updates = lists:foldl(fun
            ({id, _}, Acc) -> Acc;
            ({A, V}, Acc) -> [atom_to_list(A) ++ " = " ++ pack_value(V)|Acc]
        end, [], Record:attributes()),
    ["UPDATE ", TableName, " SET ", string:join(Updates, ", "),
        " WHERE id = ", pack_value(Id)].

build_select_query(Type, Conditions, Max, Skip, Sort, SortOrder) ->
    TableName = type_to_table_name(Type),
    ["SELECT * FROM ", TableName, 
        " WHERE ", build_conditions(Conditions),
        " ORDER BY ", atom_to_list(Sort), " ", sort_order_sql(SortOrder),
        " LIMIT ", integer_to_list(Max), 
        " OFFSET ", integer_to_list(Skip)
    ].

build_conditions(Conditions) ->
    build_conditions1(Conditions, [" TRUE"]).

build_conditions1([], Acc) ->
    Acc;
build_conditions1([{Key, 'equals', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "=", pack_value(Value)));
build_conditions1([{Key, 'not_equals', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "!=", pack_value(Value)));
build_conditions1([{Key, 'in', Value}|Rest], Acc) when is_list(Value) ->
    PackedValues = pack_set(Value),
    build_conditions1(Rest, add_cond(Acc, Key, "IN", PackedValues));
build_conditions1([{Key, 'not_in', Value}|Rest], Acc) when is_list(Value) ->
    PackedValues = pack_set(Value),
    build_conditions1(Rest, add_cond(Acc, Key, "NOT IN", PackedValues));
build_conditions1([{Key, 'in', {Min, Max}}|Rest], Acc) when Max >= Min ->
    PackedValues = pack_range(Min, Max),
    build_conditions1(Rest, add_cond(Acc, Key, "BETWEEN", PackedValues));
build_conditions1([{Key, 'not_in', {Min, Max}}|Rest], Acc) when Max >= Min ->
    PackedValues = pack_range(Min, Max),
    build_conditions1(Rest, add_cond(Acc, Key, "NOT BETWEEN", PackedValues));
build_conditions1([{Key, 'gt', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, ">", pack_value(Value)));
build_conditions1([{Key, 'lt', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "<", pack_value(Value)));
build_conditions1([{Key, 'ge', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, ">=", pack_value(Value)));
build_conditions1([{Key, 'le', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "<=", pack_value(Value)));
build_conditions1([{Key, 'matches', "*"++Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "~*", pack_value(Value)));
build_conditions1([{Key, 'not_matches', "*"++Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "!~*", pack_value(Value)));
build_conditions1([{Key, 'matches', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "~", pack_value(Value)));
build_conditions1([{Key, 'not_matches', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, "!~", pack_value(Value)));
build_conditions1([{Key, 'contains', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, pack_tsvector(Key), "@@", pack_tsquery([Value], "&")));
build_conditions1([{Key, 'not_contains', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, pack_tsvector(Key), "@@", pack_tsquery_not([Value], "&")));
build_conditions1([{Key, 'contains_all', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, pack_tsvector(Key), "@@", pack_tsquery(Values, "&")));
build_conditions1([{Key, 'not_contains_all', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, pack_tsvector(Key), "@@", pack_tsquery_not(Values, "&")));
build_conditions1([{Key, 'contains_any', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, pack_tsvector(Key), "@@", pack_tsquery(Values, "|")));
build_conditions1([{Key, 'contains_none', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, pack_tsvector(Key), "@@", pack_tsquery_not(Values, "|"))).

add_cond(Acc, Key, Op, PackedVal) ->
    [lists:concat([Key, " ", Op, " ", PackedVal, " AND "])|Acc].

pack_tsvector(Key) ->
    atom_to_list(Key) ++ "::tsvector".

pack_tsquery(Values, Op) ->
    "'" ++ string:join(lists:map(fun escape_sql/1, Values), " "++Op++" ") ++ "'::tsquery".

pack_tsquery_not(Values, Op) ->
    "'!(" ++ string:join(lists:map(fun escape_sql/1, Values), " "++Op++" ") ++ ")'::tsquery".

pack_set(Values) ->
    "(" ++ string:join(lists:map(fun pack_value/1, Values), ", ") ++ ")".

pack_range(Min, Max) ->
    pack_value(Min) ++ " AND " ++ pack_value(Max).

escape_sql(Value) ->
    escape_sql1(Value, []).

escape_sql1([], Acc) ->
    lists:reverse(Acc);
escape_sql1([$'|Rest], Acc) ->
    escape_sql1(Rest, [$', $'|Acc]);
escape_sql1([C|Rest], Acc) ->
    escape_sql1(Rest, [C|Acc]).

datetime_to_now(DateTime) ->
    GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
    {ESeconds div 1000000, ESeconds rem 1000000, 0}.

pack_value({{_, _, _}, {_, _, _}} = Val) ->
    datetime_to_now(Val);
pack_value(V) -> V.

%% Functions below copied from emongo <https://github.com/boorad/emongo>
%% 
%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com> 
%% Jacob Perkins <japerk@gmail.com> 
%% Belyaev Dmitry <rumata-estor@nm.ru> 
%% François de Metz <fdemetz@af83.com>

dec2hex(Dec) ->
    dec2hex(<<>>, Dec).

dec2hex(N, <<I:8,Rem/binary>>) ->
    dec2hex(<<N/binary, (hex0((I band 16#f0) bsr 4)):8, (hex0((I band 16#0f))):8>>, Rem);
dec2hex(N,<<>>) ->
    N.

hex2dec(Hex) when is_list(Hex) ->
    hex2dec(list_to_binary(Hex));

hex2dec(Hex) ->
    hex2dec(<<>>, Hex).

hex2dec(N,<<A:8,B:8,Rem/binary>>) ->
    hex2dec(<<N/binary, ((dec0(A) bsl 4) + dec0(B)):8>>, Rem);
hex2dec(N,<<>>) ->
    N.

dec0($a) -> 10;
dec0($b) -> 11;
dec0($c) -> 12;
dec0($d) -> 13;
dec0($e) -> 14;
dec0($f) -> 15;
dec0(X) -> X - $0.

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.

