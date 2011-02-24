% In-memory database for fast tests and easy setup
-module(boss_db_adapter_mock).
-behaviour(boss_db_adapter).
-export([start/0, start/1, stop/1]).
-export([find/2, find/7, count/3, counter/2, incr/3, delete/2, save_record/2]).
-export([push/2, pop/2, dump/1]).

start() ->
    start([]).

start(_Options) ->
    register(boss_db_mock, spawn(fun() -> loop({[], 1}) end)),
    {ok, undefined}.

stop(_) ->
    boss_db_mock ! {self(), stop},
    ok.

loop({[], IdCounter}) ->
    loop([{dict:new(), IdCounter}]);
loop([{Dict, IdCounter}|OldState] = State) ->
    receive
        {From, reset} ->
            From ! {boss_db_mock, ok},
            loop([{dict:new(), 1}|OldState]);
        {From, {find, Id}} ->
            case dict:find(Id, Dict) of
                {ok, Record} ->
                    From ! {boss_db_mock, Record};
                error ->
                    From ! {boss_db_mock, undefined}
            end,
            loop(State);
        {From, {find, Type, Conditions, Max, Skip, SortBy, SortOrder}} ->
            Records = do_find(Dict, Type, Conditions, Max, Skip, SortBy, SortOrder),
            From ! {boss_db_mock, Records},
            loop(State);
        {From, {count, Type, Conditions}} ->
            Records = do_find(Dict, Type, Conditions, 1000 * 1000 * 1000 * 1000, 0, id, str_ascending),
            From ! {boss_db_mock, length(Records)},
            loop(State);
        {From, {delete, Id}} ->
            From ! {boss_db_mock, ok},
            loop([{dict:erase(Id, Dict), IdCounter}|OldState]);
        {From, {counter, Id}} ->
            Value = case dict:find(Id, Dict) of
                {ok, Integer} when is_integer(Integer) ->
                    Integer;
                _ ->
                    0
            end,
            From ! {boss_db_mock, Value},
            loop(State);
        {From, {incr, Id, Amount}} ->
            NewValue = case dict:find(Id, Dict) of
                {ok, OldValue} when is_integer(OldValue) ->
                    OldValue + Amount;
                _ ->
                    Amount
            end,
            From ! {boss_db_mock, NewValue},
            loop([{dict:store(Id, NewValue, Dict), IdCounter}|OldState]);
        {From, {save_record, Record}} ->
            Type = element(1, Record),
            {Id, IdCounter1} = case Record:id() of
                id -> {lists:concat([Type, "-", IdCounter]), IdCounter + 1};
                Other -> {Other, IdCounter}
            end,
            NewAttributes = lists:map(fun
                    ({id, _}) ->
                        {id, Id};
                    ({Attr, {_, _, _} = Val}) ->
                        {Attr, calendar:now_to_datetime(Val)};
                    (Other) ->
                        Other
                end, Record:attributes()),
            NewRecord = Record:attributes(NewAttributes),
            From ! {boss_db_mock, NewRecord},
            loop([{dict:store(Id, NewRecord, Dict), IdCounter1}|OldState]);
        {From, push} ->
            From ! {boss_db_mock, ok},
            loop([{Dict, IdCounter}|State]);
        {From, pop} ->
            From ! {boss_db_mock, ok},
            loop(OldState);
        {From, dump} ->
            From ! {boss_db_mock, dict:to_list(Dict)},
            loop(State);
        {_From, stop} ->
            ok
    end.

find(_, Id) ->
    boss_db_mock ! {self(), {find, Id}},
    receive
        {boss_db_mock, Record} ->
            Record
    end.

find(_, Type, Conditions, Max, Skip, SortBy, SortOrder) ->
    boss_db_mock ! {self(), {find, Type, Conditions, Max, Skip, SortBy, SortOrder}},
    receive
        {boss_db_mock, Records} ->
            Records
    end.

count(_, Type, Conditions) ->
    boss_db_mock ! {self(), {count, Type, Conditions}},
    receive
        {boss_db_mock, Count} ->
            Count
    end.

counter(_, Id) ->
    boss_db_mock ! {self(), {counter, Id}},
    receive
        {boss_db_mock, Count} ->
            Count
    end.

incr(_, Id, Amount) ->
    boss_db_mock ! {self(), {incr, Id, Amount}},
    receive
        {boss_db_mock, Int} -> Int
    end.

delete(_, Id) ->
    boss_db_mock ! {self(), {delete, Id}},
    receive
        {boss_db_mock, ok} ->
            ok
    end.

save_record(_, Record) ->
    boss_db_mock ! {self(), {save_record, Record}},
    receive
        {boss_db_mock, SavedRecord} ->
            {ok, SavedRecord}
    end.

push(_, _Depth) ->
    boss_db_mock ! {self(), push},
    receive
        {boss_db_mock, ok} ->
            ok
    end.

pop(_, _Depth) ->
    boss_db_mock ! {self(), pop},
    receive
        {boss_db_mock, ok} ->
            ok
    end.

dump(_) ->
    boss_db_mock ! {self(), dump},
    receive
        {boss_db_mock, Data} ->
            Data
    end.


% internal

do_find(Dict, Type, Conditions, Max, Skip, SortBy, SortOrder) ->
    lists:sublist(lists:nthtail(Skip, 
            lists:sort(fun(RecordA, RecordB) ->
                        AttributeA = sortable_attribute(RecordA, SortBy),
                        AttributeB = sortable_attribute(RecordB, SortBy),
                        case SortOrder of
                            str_ascending ->
                                AttributeA < AttributeB;
                            str_descending ->
                                AttributeA > AttributeB;
                            num_ascending ->
                                AttributeA < AttributeB;
                            num_descending ->
                                AttributeA > AttributeB
                        end
                end,
                lists:map(fun({_, V}) -> V end,
                    dict:to_list(dict:filter(
                            fun(_Id, Record) when is_tuple(Record) ->
                                    element(1, Record) =:= Type andalso
                                    match_cond(Record, Conditions);
                                (_Id, _) ->
                                    false
                            end, Dict))))), Max).

match_cond(_Record, []) ->
    true;
match_cond(Record, [{Key, 'equals', Value}|Rest]) ->
    Record:Key() =:= Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_equals', Value}|Rest]) ->
    Record:Key() =/= Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'in', Value}|Rest]) when is_list(Value) ->
    lists:member(Record:Key(), Value) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_in', Value}|Rest]) when is_list(Value) ->
    (not lists:member(Record:Key(), Value)) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'in', {Min, Max}}|Rest]) when Max >= Min ->
    Record:Key() >= Min andalso Record:Key() =< Max andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_in', {Min, Max}}|Rest]) when Max >= Min ->
    (not (Record:Key() >= Min andalso Record:Key() =< Max)) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'gt', Value}|Rest]) ->
    Record:Key() > Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'lt', Value}|Rest]) ->
    Record:Key() < Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'ge', Value}|Rest]) ->
    Record:Key() >= Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'le', Value}|Rest]) ->
    Record:Key() =< Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'matches', "*"++Value}|Rest]) ->
    re:run(Record:Key(), Value, [caseless]) =/= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'matches', Value}|Rest]) ->
    re:run(Record:Key(), Value, []) =/= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_matches', "*"++Value}|Rest]) ->
    re:run(Record:Key(), Value, [caseless]) =:= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_matches', Value}|Rest]) ->
    re:run(Record:Key(), Value, []) =:= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains', Value}|Rest]) ->
    lists:member(Value, string:tokens(Record:Key(), " ")) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_contains', Value}|Rest]) ->
    (not lists:member(Value, string:tokens(Record:Key(), " "))) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains_all', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    lists:all(fun(Token) -> lists:member(Token, Tokens) end, Value) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_contains_all', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    (not lists:all(fun(Token) -> lists:member(Token, Tokens) end, Value)) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains_any', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    lists:any(fun(Token) -> lists:member(Token, Tokens) end, Value) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains_none', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    (not lists:any(fun(Token) -> lists:member(Token, Tokens) end, Value)) andalso match_cond(Record, Rest).

sortable_attribute(Record, Attr) ->
    case Record:Attr() of
        {D, T} when is_tuple(D), is_tuple(T) ->
            calendar:datetime_to_gregorian_seconds({D, T});
        Now when is_tuple(Now) ->
            calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now));
        Bin when is_binary(Bin) ->
            binary_to_list(Bin);
        true -> 1;
        false -> 0;
        Other ->
            Other
    end.

