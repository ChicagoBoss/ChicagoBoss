-module(boss_db_adapter_tyrant).
-behaviour(boss_db_adapter).
-export([start/0, start/1, stop/1, find/2, find/7]).
-export([count/3, counter/2, incr/3, delete/2, save_record/2]).

-define(TRILLION, (1000 * 1000 * 1000 * 1000)).

start() ->
    start([]).

start(Options) ->
    Host = proplists:get_value(db_host, Options, "localhost"),
    Port = proplists:get_value(db_port, Options, 1978),
    ok = medici:start([{hostname, Host}, {port, Port}]),
    {ok, undefined}.

stop(_) ->
    medici:stop().

find(_, Id) when is_list(Id) ->
    Type = infer_type_from_id(Id),
    case medici:get(list_to_binary(Id)) of
        Record when is_list(Record) ->
            case boss_record_lib:ensure_loaded(Type) of
                true -> activate_record(Record, Type);
                false -> {error, {module_not_loaded, Type}}
            end;
        {error, invalid_operation} ->
            undefined;
        {error, Reason} ->
            {error, Reason}
    end.

find(_, Type, Conditions, Max, Skip, Sort, SortOrder) when is_atom(Type), is_list(Conditions), is_integer(Max),
                                                        is_integer(Skip), is_atom(Sort), is_atom(SortOrder) ->
    case boss_record_lib:ensure_loaded(Type) of
        true ->
            Query = build_query(Type, Conditions, Max, Skip, Sort, SortOrder),
            lists:map(fun({_Id, Record}) -> activate_record(Record, Type) end,
                medici:mget(medici:search(Query)));
        false ->
            []
    end.

count(_, Type, Conditions) ->
    medici:searchcount(build_conditions(Type, Conditions)).

counter(_, Id) when is_list(Id) ->
    case medici:get(list_to_binary(Id)) of
        Record when is_list(Record) ->
            list_to_integer(binary_to_list(
                    proplists:get_value(<<"_num">>, Record, <<"0">>)));
        {error, _Reason} -> 0
    end.

incr(_, Id, Count) when is_list(Id) ->
    medici:addint(list_to_binary(Id), Count).

delete(_, Id) when is_list(Id) ->
    medici:out(list_to_binary(Id)).

save_record(_, Record) when is_tuple(Record) ->
    Type = element(1, Record),
    Id = case Record:id() of
        id ->
            atom_to_list(Type) ++ "-" ++ binary_to_list(medici:genuid());
        Defined when is_list(Defined) ->
            Defined
    end,
    RecordWithId = Record:id(Id),
    PackedRecord = pack_record(RecordWithId, Type),

    Result = medici:put(list_to_binary(Id), PackedRecord),
    case Result of
        ok -> {ok, RecordWithId};
        {error, Error} -> {error, [Error]}
    end.

% internal

pack_record(RecordWithId, Type) ->
    % metadata string stores the data types, <Data Type><attribute name>
    {Columns, MetadataString} = lists:mapfoldl(fun
            (Attr, Acc) -> 
                Val = RecordWithId:Attr(),
                MagicLetter = case Val of
                    _Val when is_list(_Val) ->    "L";
                    {_, _, _} ->                  "T"; % time
                    {{_, _, _}, {_, _, _}} ->     "T";
                    _Val when is_binary(_Val) ->  "B";
                    _Val when is_integer(_Val) -> "I"; 
                    _Val when is_float(_Val) ->   "F";
                    _Val when is_boolean(_Val) -> "O"
                end,
                {{attribute_to_colname(Attr), pack_value(Val)}, 
                    lists:concat([Attr, MagicLetter, Acc])}
        end, [], RecordWithId:attribute_names()),
    [{attribute_to_colname('_type'), list_to_binary(atom_to_list(Type))},
        {attribute_to_colname('_metadata'), list_to_binary(MetadataString)}|Columns].

infer_type_from_id(Id) when is_binary(Id) ->
    infer_type_from_id(binary_to_list(Id));
infer_type_from_id(Id) when is_list(Id) ->
    list_to_atom(hd(string:tokens(Id, "-"))).

extract_metadata(Record, _Type) ->
    MetadataString = proplists:get_value(attribute_to_colname('_metadata'), Record),
    case MetadataString of
        undefined -> [];
        Val when is_binary(Val) -> 
            parse_metadata_string(binary_to_list(Val))
    end.

parse_metadata_string(Val) ->
    parse_metadata_string(Val, [], []).
parse_metadata_string([], [], MetadataAcc) ->
    MetadataAcc;
parse_metadata_string([H|T], NameAcc, MetadataAcc) when H >= $A, H =< $Z ->
    parse_metadata_string(T, [], [{list_to_atom(lists:reverse(NameAcc)), H}|MetadataAcc]);
parse_metadata_string([H|T], NameAcc, MetadataAcc) when H >= $a, H =< $z; H =:= $_; H >= $0, H =< $9 ->
    parse_metadata_string(T, [H|NameAcc], MetadataAcc).

activate_record(Record, Type) ->
    Metadata = extract_metadata(Record, Type),
    apply(Type, new, lists:map(fun
                (Key) ->
                    Val = proplists:get_value(attribute_to_colname(Key), Record, <<"">>),
                    case proplists:get_value(Key, Metadata) of
                        $B -> Val;
                        $I -> list_to_integer(binary_to_list(Val));
                        $F -> list_to_integer(binary_to_list(Val)) / ?TRILLION;
                        $T -> unpack_datetime(Val);
                        $O -> Val =:= <<"1">>;
                        _ ->
                            case lists:suffix("_time", atom_to_list(Key)) of
                                true -> unpack_datetime(Val);
                                false -> binary_to_list(Val)
                            end
                    end
            end, boss_record_lib:attribute_names(Type))).

attribute_to_colname(Attribute) ->
    list_to_binary(atom_to_list(Attribute)).

build_query(Type, Conditions, Max, Skip, Sort, SortOrder) ->
    Query = build_conditions(Type, Conditions),
    medici:query_order(medici:query_limit(Query, Max, Skip), atom_to_list(Sort), SortOrder).

build_conditions(Type, Conditions) ->
    build_conditions1([{'_type', 'equals', atom_to_list(Type)}|Conditions], []).

build_conditions1([], Acc) ->
    Acc;
build_conditions1([{Key, 'equals', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_eq, pack_value(Value)));
build_conditions1([{Key, 'not_equals', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_eq}, pack_value(Value)));
build_conditions1([{Key, 'in', Value}|Rest], Acc) when is_list(Value) ->
    PackedValues = pack_tokens(Value),
    build_conditions1(Rest, add_cond(Acc, Key, str_in_list, PackedValues));
build_conditions1([{Key, 'not_in', Value}|Rest], Acc) when is_list(Value) ->
    PackedValues = pack_tokens(Value),
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_in_list}, PackedValues));
build_conditions1([{Key, 'in', {Min, Max}}|Rest], Acc) when Max >= Min ->
    PackedValues = pack_tokens([Min, Max]),
    build_conditions1(Rest, add_cond(Acc, Key, num_between, PackedValues));
build_conditions1([{Key, 'not_in', {Min, Max}}|Rest], Acc) when Max >= Min ->
    PackedValues = pack_tokens([Min, Max]),
    build_conditions1(Rest, add_cond(Acc, Key, {no, num_between}, PackedValues));
build_conditions1([{Key, 'gt', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_gt, pack_value(Value)));
build_conditions1([{Key, 'lt', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_lt, pack_value(Value)));
build_conditions1([{Key, 'ge', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_ge, pack_value(Value)));
build_conditions1([{Key, 'le', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, num_le, pack_value(Value)));
build_conditions1([{Key, 'matches', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_regex, pack_value(Value)));
build_conditions1([{Key, 'not_matches', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_regex}, pack_value(Value)));
build_conditions1([{Key, 'contains', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_and, pack_value(Value)));
build_conditions1([{Key, 'not_contains', Value}|Rest], Acc) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_and}, pack_value(Value)));
build_conditions1([{Key, 'contains_all', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_and, pack_tokens(Values)));
build_conditions1([{Key, 'not_contains_all', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_and}, pack_tokens(Values)));
build_conditions1([{Key, 'contains_any', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, str_or, pack_tokens(Values)));
build_conditions1([{Key, 'contains_none', Values}|Rest], Acc) when is_list(Values) ->
    build_conditions1(Rest, add_cond(Acc, Key, {no, str_or}, pack_tokens(Values))).

add_cond(Acc, Key, Op, PackedVal) ->
    medici:query_add_condition(Acc, attribute_to_colname(Key), Op, [PackedVal]).

pack_tokens(Tokens) ->
    list_to_binary(string:join(lists:map(fun(V) -> binary_to_list(pack_value(V)) end, Tokens), " ")).

pack_datetime({Date, Time}) ->
    list_to_binary(integer_to_list(calendar:datetime_to_gregorian_seconds({Date, Time}))).

pack_now(Now) -> pack_datetime(calendar:now_to_datetime(Now)).

pack_value(V) when is_binary(V) ->
    V;
pack_value(V) when is_list(V) ->
    list_to_binary(V);
pack_value({MegaSec, Sec, MicroSec}) when is_integer(MegaSec) andalso is_integer(Sec) andalso is_integer(MicroSec) ->
    pack_now({MegaSec, Sec, MicroSec});
pack_value({{_, _, _}, {_, _, _}} = Val) ->
    pack_datetime(Val);
pack_value(Val) when is_integer(Val) ->
    list_to_binary(integer_to_list(Val));
pack_value(Val) when is_float(Val) ->
    list_to_binary(integer_to_list(trunc(Val * ?TRILLION)));
pack_value(true) ->
    <<"1">>;
pack_value(false) ->
    <<"0">>.

unpack_datetime(<<"">>) -> calendar:gregorian_seconds_to_datetime(0);
unpack_datetime(Bin) -> calendar:universal_time_to_local_time(
        calendar:gregorian_seconds_to_datetime(list_to_integer(binary_to_list(Bin)))).
