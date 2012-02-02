-module(boss_record).
-export([new/2]).

new(Model, Attributes) ->
    DummyRecord = boss_record_lib:dummy_record(Model),
    DummyRecord:set(Attributes).
