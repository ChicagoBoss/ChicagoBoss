-module(boss_record_lib).
-compile(export_all).


run_before_hooks(Record, true) ->
    run_hooks(Record, element(1, Record), before_create);
run_before_hooks(Record, false) ->
    run_hooks(Record, element(1, Record), before_update).

run_after_hooks(Record, true) ->
    run_hooks(Record, element(1, Record), after_create);
run_after_hooks(Record, false) ->
    run_hooks(Record, element(1, Record), after_update).

run_hooks(Record, Type, Function) ->
    case erlang:function_exported(Type, Function, 1) of
        true -> Record:Function();
        false -> ok
    end.
