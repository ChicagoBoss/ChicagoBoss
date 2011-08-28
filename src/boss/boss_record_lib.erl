-module(boss_record_lib).
-compile(export_all).


run_before_hooks(Record, true) ->
    run_hooks(Record, element(1, Record), before_create);
run_before_hooks(Record, false) ->
    run_hooks(Record, element(1, Record), before_update).

run_after_hooks(_UnsavedRecord, SavedRecord, true) ->
    boss_news:created(SavedRecord:id(), SavedRecord:attributes()),
    run_hooks(SavedRecord, element(1, SavedRecord), after_create);
run_after_hooks(UnsavedRecord, SavedRecord, false) ->
    boss_news:updated(SavedRecord:id(), UnsavedRecord:attributes(), SavedRecord:attributes()),
    run_hooks(SavedRecord, element(1, SavedRecord), after_update).

run_before_delete_hooks(Record) ->
    run_hooks(Record, element(1, Record), before_delete).

run_hooks(Record, Type, Function) ->
    case erlang:function_exported(Type, Function, 1) of
        true -> Record:Function();
        false -> ok
    end.

is_boss_record(Record, ModelList) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
    Type = element(1, Record),
    lists:member(atom_to_list(Type), ModelList) andalso 
        erlang:function_exported(Type, attribute_names, 1) andalso 
        erlang:function_exported(Type, new, tuple_size(Record) - 1);
is_boss_record(_, _) ->
    false.

dummy_record(Module) ->
    NumArgs = proplists:get_value('new', Module:module_info(exports)),
    apply(Module, 'new', lists:map(fun(1) -> 'id'; (_) -> "" end, lists:seq(1, NumArgs))).

attribute_names(Module) ->
    DummyRecord = dummy_record(Module),
    DummyRecord:attribute_names().
