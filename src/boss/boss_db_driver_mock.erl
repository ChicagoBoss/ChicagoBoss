% In-memory database for fast tests and easy setup
-module(boss_db_driver_mock).
-behaviour(boss_db_driver).
-export([start/0, stop/0, find/1, find/3, find/4, find/5, find/6]).
-export([count/1, count/2, counter/1, incr/1, incr/2, delete/1, save_record/1]).

start() ->
    register(boss_db_mock, spawn(fun() -> loop(dict:new(), 1) end)).

stop() ->
    unregister(boss_db_mock).

loop(Dict, IdCounter) ->
    receive
        {From, reset} ->
            From ! {boss_db_mock, ok},
            loop(dict:new(), 1);
        {From, {find, Id}} ->
            case dict:find(Id, Dict) of
                {ok, Record} ->
                    From ! {boss_db_mock, Record};
                error ->
                    From ! {boss_db_mock, undefined}
            end,
            loop(Dict, IdCounter);
        {From, {find, Type, Conditions, Max, Skip, SortBy, SortOrder}} ->
            Records = do_find(Dict, Type, Conditions, Max, Skip, SortBy, SortOrder),
            From ! {boss_db_mock, Records},
            loop(Dict, IdCounter);
        {From, {count, Type, Conditions}} ->
            Records = do_find(Dict, Type, Conditions, 0, 0, id, str_ascending),
            From ! {boss_db_mock, length(Records)},
            loop(Dict, IdCounter);
        {From, {delete, Id}} ->
            From ! ok,
            loop(dict:erase(Id, Dict), IdCounter);
        {From, {counter, Id}} ->
            Value = case dict:find(Id, Dict) of
                {ok, Integer} when is_integer(Integer) ->
                    Integer;
                _ ->
                    0
            end,
            From ! {boss_db_mock, Value},
            loop(Dict, IdCounter);
        {From, {incr, Id, Amount}} ->
            NewValue = case dict:find(Id, Dict) of
                {ok, OldValue} when is_integer(OldValue) ->
                    OldValue + Amount;
                _ ->
                    Amount
            end,
            From ! {boss_db_mock, NewValue},
            loop(dict:store(Id, NewValue, Dict), IdCounter);
        {From, {save_record, Record}} ->
            Type = element(1, Record),
            {Id, IdCounter1} = case Record:id() of
                id -> {lists:concat([Type, "-", IdCounter]), IdCounter + 1};
                Other -> {Other, IdCounter}
            end,
            NewRecord = Record:id(Id),
            From ! {boss_db_mock, NewRecord},
            loop(dict:store(Record:id(Id), Dict), IdCounter1)
    end.

find(Id) ->
    boss_db_mock ! {self(), {find, Id}},
    receive
        {boss_db_mock, Record} ->
            Record
    end.

find(Type, Conditions, Max) ->
    find(Type, Conditions, Max, 0).
find(Type, Conditions, Max, Skip) ->
    find(Type, Conditions, Max, Skip, id).
find(Type, Conditions, Max, Skip, SortBy) ->
    find(Type, Conditions, Max, Skip, SortBy, str_ascending).
find(Type, Conditions, Max, Skip, SortBy, SortOrder) ->
    boss_db_mock ! {self(), {find, Type, Conditions, Max, Skip, SortBy, SortOrder}},
    receive
        {boss_db_mock, Records} ->
            Records
    end.

count(Type) ->
    count(Type, []).

count(Type, Conditions) ->
    boss_db_mock ! {self(), {count, Type, Conditions}},
    receive
        {boss_db_mock, Count} ->
            Count
    end.

counter(Id) ->
    boss_db_mock ! {self(), {counter, Id}},
    receive
        {boss_db_mock, Count} ->
            Count
    end.

incr(Id) ->
    incr(Id, 1).
incr(Id, Amount) ->
    boss_db_mock ! {self(), {incr, Id, Amount}},
    receive
        {boss_db_mock, Int} -> Int
    end.

delete(Id) ->
    boss_db_mock ! {self(), {delete, Id}},
    receive
        {boss_db_mock, ok} ->
            ok
    end.

save_record(Record) ->
    boss_db_mock ! {self(), {save_record, Record}},
    receive
        {boss_db_mock, SavedRecord} ->
            {ok, SavedRecord}
    end.

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
                                    length(Record:attributes() -- Conditions) =:= 
                                    length(Record:attributes()) - length(Conditions);
                                (_Id, _) ->
                                    false
                            end, Dict))))), Max).

sortable_attribute(Record, Attr) ->
    case Record:Attr() of
        {D, T} when is_tuple(D), is_tuple(T) ->
            calendar:datetime_to_gregorian_seconds({D, T});
        Now when is_tuple(Now) ->
            calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now));
        Bin when is_binary(Bin) ->
            binary_to_list(Bin);
        Other ->
            Other
    end.

