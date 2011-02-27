-module(boss_db_test_model, [Id, SomeText, SomeTime, SomeBoolean, SomeInteger, SomeFloat, BossDbTestParentModelId]).
-compile(export_all).

-belongs_to(boss_db_test_parent_model).
