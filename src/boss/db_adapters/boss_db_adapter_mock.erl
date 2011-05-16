% In-memory database for fast tests and easy setup
-module(boss_db_adapter_mock).
-behaviour(boss_db_adapter).
-export([start/0, start/1, stop/1]).
-export([find/2, find/7, count/3, counter/2, incr/3, delete/2, save_record/2]).
-export([push/2, pop/2, dump/1, transaction/2]).

start() ->
    start([]).

start(_Options) ->
    {ok, MockSup} = boss_db_mock_sup:start_link(),
    {ok, MockSup}.

stop(MockSup) ->
    exit(MockSup),
    ok.

find(_, Id) ->
    gen_server:call(boss_db_mock, {find, Id}).

find(_, Type, Conditions, Max, Skip, SortBy, SortOrder) ->
    gen_server:call(boss_db_mock, {find, Type, Conditions, Max, Skip, SortBy, SortOrder}).

count(_, Type, Conditions) ->
    gen_server:call(boss_db_mock, {count, Type, Conditions}).

counter(_, Id) ->
    gen_server:call(boss_db_mock, {counter, Id}).

incr(_, Id, Amount) ->
    gen_server:call(boss_db_mock, {incr, Id, Amount}).

delete(_, Id) ->
    gen_server:call(boss_db_mock, {delete, Id}).

save_record(_, Record) ->
    SavedRecord = gen_server:call(boss_db_mock, {save_record, Record}),
    {ok, SavedRecord}.

push(_, _Depth) ->
    gen_server:call(boss_db_mock, push).

pop(_, _Depth) ->
    gen_server:call(boss_db_mock, pop).

dump(_) ->
    gen_server:call(boss_db_mock, dump).

transaction(_, TransactionFun) ->
    State = gen_server:call(boss_db_mock, get_state),
    try
        {atomic, TransactionFun()}
    catch
        _:Why ->
            gen_server:call(boss_db_mock, {set_state, State}),
            {aborted, Why}
    end.
