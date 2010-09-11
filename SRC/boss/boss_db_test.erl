-module(boss_db_test).
-compile(export_all).

start() ->
    start(["mock"]).

start([Adapter]) ->
    AdapterMod = list_to_atom("boss_db_adapter_"++Adapter),
    application:set_env(boss_db_test, db_adapter, AdapterMod),
    application:start(boss_db_test).

stop() ->
    application:stop(boss_db_test).
