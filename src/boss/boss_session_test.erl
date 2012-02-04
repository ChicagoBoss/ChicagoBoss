-module(boss_session_test).
-export([start/0, start/1, stop/0]).

start() ->
    start([]).

start([]) ->
    application:start(boss_session_test).

stop() ->
    application:stop(boss_session_test).
