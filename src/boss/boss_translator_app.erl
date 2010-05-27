-module(boss_translator_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    boss_translator_sup:start_link(StartArgs).

stop(_State) ->
    ok.
