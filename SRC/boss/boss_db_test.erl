-module(boss_db_test).
-compile(export_all).

start() ->
    start([]).

start([]) ->
    application:start(boss_db_test).

stop() ->
    application:stop(boss_db_test).
