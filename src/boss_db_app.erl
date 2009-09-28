-module(boss_db_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    boss_db_sup:start_link(StartArgs).

stop(_State) ->
    ok.
