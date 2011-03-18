-module(boss_session_test).
-compile(export_all).

start() ->
    start([]).

start([]) ->
    application:start(boss_session_test).

stop() ->
    application:stop(boss_session_test).
