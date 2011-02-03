-module(boss_db_adapter_mongodb).
-behaviour(boss_db_adapter).
-export([start/0, start/1, stop/1, find/2, find/7]).
-export([count/3, counter/2, incr/2, incr/3, delete/2, save_record/2]).
-export([execute/2]).
-export([push/2, pop/2]).

-define(LOG(Name, Value), io:format("DEBUG: ~s: ~p~n", [Name, Value])).

% Number of seconds between beginning of gregorian calendar and 1970
-define(GREGORIAN_SECONDS_1970, 62167219200). 

% JavaScript expression formats to query MongoDB
-define(CONTAINS_FORMAT, "this.~s.indexOf('~s') != -1").
-define(NOT_CONTAINS_FORMAT, "this.~s.indexOf('~s') == -1").


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

find(Conn, Type, Conditions, Max, Skip, Sort, SortOrder) when is_atom(Type), is_list(Conditions), 
                                                              is_integer(Max), is_integer(Skip), 
                                                              is_atom(Sort), is_atom(SortOrder) ->
%    ?LOG("find Type", Type),
%    ?LOG("find Conditions", Conditions),
    case model_is_loaded(Type) of
        true ->
            Collection = type_to_collection(Type),
            Res = execute(Conn, fun() ->
                    SortOrder1 = case SortOrder of 
                        str_ascending -> 1;
                        num_ascending -> 1;
                        str_descending -> -1;
                        num_descending -> -1
                    end,
                    mongo:find(Collection, build_conditions(Conditions), [],
                               Skip, Max, {Sort, SortOrder1}) 
                end),
            case Res of
                {ok, Curs} ->
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
                C = build_conditions(Conditions),
%                ?LOG("Conditions", C),
                mongo:count(Collection, C)
        end),
%    ?LOG("Count", Count),
    Count.

counter(Conn, Id) when is_list(Id) ->
    Res = execute(Conn, fun() ->
                mongo:find_one(boss_counters, {'name', list_to_binary(Id)})
        end),
    case Res of
        {ok, {Doc}} -> 
            PropList = tuple_to_proplist(Doc),
            proplists:get_value(value, PropList);
        {failure, Reason} -> {error, Reason};
        {connection_failure, Reason} -> {error, Reason}
    end.

incr(Conn, Id) ->
    incr(Conn, Id, 1).

incr(Conn, Id, Count) ->
    Res = execute(Conn, fun() -> 
                 mongo:repsert(boss_counters, 
                         {'name', list_to_binary(Id)},
                         {'$inc', {value, Count}}
                         )
        end),
    case Res of
        {ok, ok} -> counter(Conn, Id);
        {failure, Reason} -> {error, Reason};
        {connection_failure, Reason} -> {error, Reason}
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


is_id_attr(AttrName) ->
    lists:suffix("_id", atom_to_list(AttrName)).

save_record(Conn, Record) when is_tuple(Record) ->
    Type = element(1, Record),
    Collection = type_to_collection(Type),
    Attributes = case Record:id() of
        id ->
            PropList = lists:map(fun({K,V}) -> 
                            case is_id_attr(K) of
                                true -> {K, boss_to_mongo_id(V)};
                                false -> {K, pack_value(V)}
                            end
                end, 
                                 Record:attributes()),
            proplist_to_tuple(proplists:delete(id, PropList));
        DefinedId when is_list(DefinedId) ->
            PropList = lists:map(fun({K,V}) ->
                            case K of 
                                id -> {'_id', boss_to_mongo_id(DefinedId)};
                                _ -> 
                                    case is_id_attr(K) of
                                        true -> {K, boss_to_mongo_id(V)};
                                        false -> {K, pack_value(V)}
                                    end
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

% These 2 functions are not part of the behaviour but are required for
% tests to pass
push(_Conn, _Depth) -> ok.

pop(_Conn, _Depth) -> ok.

%%
%% Internal functions
%%

boss_to_mongo_op(BossOperator) ->
    OperatorsMap = [
        {'not_equals', '$ne'},
        {'gt', '$gt'},
        {'ge', '$gte'},
        {'lt', '$lt'},
        {'le', '$lte'},
        {'in', '$in'},
        {'not_in', '$nin'}
    ],
    proplists:get_value(BossOperator, OperatorsMap).

where_clause(Format, Params) ->
    erlang:iolist_to_binary(
                io_lib:format(Format, Params)).

multiple_where_clauses_string(Format, Key, ValueList, Operator) ->
    ClauseList = lists:map(fun(Value) ->
                lists:flatten(io_lib:format(Format, [Key, Value]))
        end, ValueList),
        string:join(ClauseList, " " ++ Operator ++ " ").

multiple_where_clauses(Format, Key, ValueList, Operator) ->
    erlang:iolist_to_binary(multiple_where_clauses_string(Format, Key,
            ValueList, Operator)).

build_conditions(Conditions) ->
    proplist_to_tuple(build_conditions1(Conditions, [])).

build_conditions1([], Acc) ->
    Acc;

build_conditions1([{Key, Operator, Value}|Rest], Acc) ->
%    ?LOG("Key, Operator, Value", {Key, Operator, Value}),

    Condition = case {Operator, Value} of 
        {'not_matches', Value} ->
            [{Key, {'$not', {regex, list_to_binary(Value), <<"">>}}}];
        {'matches', Value} ->
            [{Key, {regex, list_to_binary(Value), <<"">>}}];
        {'contains', Value} ->
            WhereClause = where_clause(
                ?CONTAINS_FORMAT, [Key, Value]),
            [{'$where', WhereClause}];
        {'not_contains', Value} ->
            WhereClause = where_clause(
                ?NOT_CONTAINS_FORMAT, [Key, Value]),
            [{'$where', WhereClause}];
        {'contains_all', ValueList} ->
            WhereClause = multiple_where_clauses(
                ?CONTAINS_FORMAT, Key, ValueList, "&&"),
            [{'$where', WhereClause}];
        {'not_contains_all', ValueList} ->
            WhereClause = "!(" ++ multiple_where_clauses_string(
                ?CONTAINS_FORMAT, Key, ValueList, "&&") ++ ")",
            [{'$where', erlang:iolist_to_binary(WhereClause)}];
        {'contains_any', ValueList} ->
            WhereClause = multiple_where_clauses(
                ?CONTAINS_FORMAT, Key, ValueList, "||"),
            [{'$where', WhereClause}];
        {'contains_none', ValueList} ->
            WhereClause = multiple_where_clauses(
                ?NOT_CONTAINS_FORMAT, Key, ValueList, "&&"),
            [{'$where', WhereClause}];
        {'equals', Value} when is_list(Value) -> 
            case is_id_attr(Key) of 
                true -> 
                    [{Key, boss_to_mongo_id(Value)}];
                false -> 
                    [{Key, list_to_binary(Value)}]
            end;
        {'not_equals', Value} when is_list(Value) -> 
            [{Key, {'$ne', list_to_binary(Value)}}];
        {'equals', {{_,_,_},{_,_,_}} = Value} -> 
            [{Key, datetime_to_now(Value)}];
        {'equals', Value} -> 
            [{Key, Value}];
        {Operator, {{_,_,_},{_,_,_}} = Value} -> 
            [{Key, {boss_to_mongo_op(Operator), datetime_to_now(Value)}}];
        {'in', {Min, Max}} -> 
            [{Key, {'$gte', Min}}, {Key, {'$lte', Max}}];
        {'not_in', {Min, Max}} -> 
            [{'$or', [{Key, {'$lt', Min}}, {Key, {'$gt', Max}}]}];
        {Operator, Value} -> 
            [{Key, {boss_to_mongo_op(Operator), Value}}]
    end,
%    ?LOG("Condition", Condition),
    build_conditions1(Rest, lists:append(Condition, Acc)).


proplist_to_tuple(PropList) ->
    ListOfLists = lists:reverse([[K,V]||{K,V} <- PropList]),
    list_to_tuple(lists:foldl(
            fun([K, V], Acc) ->
                    [K,V|Acc] 
            end, [], ListOfLists)).

type_to_collection(Type) ->
    list_to_atom(type_to_table_name(Type)).

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
    try
        [_, MongoId] = string:tokens(BossId, "-"),
        {hex2dec(MongoId)}
    catch
        Error:Reason -> 
            error_logger:warning_msg("Error parsing Boss record id: ~p:~p~n", 
                [Error, Reason]),
            []
    end.

id_type_from_foreign_key(ForeignKey) ->
    Tokens = string:tokens(atom_to_list(ForeignKey), "_"),
    NameTokens = lists:filter(fun(Token) -> Token =/= "id" end, 
        Tokens),
    string:join(NameTokens, "-").

attr_value(id, MongoDoc) ->
    proplists:get_value('_id', MongoDoc);
attr_value(AttrName, MongoDoc) ->
    proplists:get_value(AttrName, MongoDoc).

mongo_to_boss_value(id, Value, RecordType) ->
    mongo_to_boss_id(RecordType, Value);
mongo_to_boss_value(AttrName, {_,_,_} = Value, _) ->
    case lists:suffix("_time", atom_to_list(AttrName)) of
        true -> calendar:now_to_datetime(Value);
        false -> Value
    end;
mongo_to_boss_value(AttrName, Value, _RecordType) ->
    case is_id_attr(AttrName) and (Value =/= "") of 
        true -> 
            IdType = id_type_from_foreign_key(AttrName),
            mongo_to_boss_id(IdType, Value);
        false -> 
            Value
    end.

attribute_names(Type) ->
    DummyRecord = apply(Type, new, lists:seq(1, proplists:get_value(new, Type:module_info(exports)))),
    DummyRecord:attribute_names().

mongo_tuple_to_record(Type, Row) ->
    MongoDoc = tuple_to_proplist(Row),
    Args = lists:map(fun(AttrName) ->
                MongoValue = attr_value(AttrName, MongoDoc),
                mongo_to_boss_value(AttrName, MongoValue, Type)
        end, attribute_names(Type)),
    apply(Type, new, Args).

infer_type_from_id(Id) when is_list(Id) ->
    [Type, _BossId] = string:tokens(Id, "-"),
    {list_to_atom(Type), type_to_collection(Type), boss_to_mongo_id(Id)}.

type_to_table_name(Type) when is_atom(Type) ->
    type_to_table_name(atom_to_list(Type));
type_to_table_name(Type) when is_list(Type) ->
    inflector:pluralize(Type).

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

