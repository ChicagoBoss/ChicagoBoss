-module(boss_db_test_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  boss_db:start(),
  run_setup(),
  run_tests(),
  erlang:halt().

stop(_State) ->
  ok.

run_setup() ->
  ok = boss_record_compiler:compile("SRC/boss/db_adapters/test_models/boss_db_test_model.erl"),
  DBAdapter = case application:get_env(db_adapter) of
    {ok, Val} -> Val;
    _ -> mock
  end,
  case file:read_file(lists:concat(["SRC/boss/db_adapters/test_sql/", DBAdapter, ".sql"])) of
    {ok, FileContents} ->
      io:format("Running setup SQL...~n", []),
      RetVal = boss_db:execute(FileContents),
      io:format("Returned: ~p~n", [RetVal]);
    {error, _Reason} ->
      ok
  end.

run_tests() ->
  io:format("~-60s", ["Root test"]),
  ModelText = <<"Economists do it with models">>,
  do(
    fun() ->
        Model = boss_db_test_model:new(id, ModelText, 
          {{1776, 7, 4}, {0, 0, 0}}, true, 42, 3.14159),
        {ok, SavedModel} = Model:save(),
        SavedModel
    end, 
    [
      fun(SavedModel) ->
          {SavedModel:id() =/= id,
            "ID field not generated"}
      end,
      fun(SavedModel) ->
          {SavedModel:some_text() =:= ModelText,
            "Text field not saved correctly"}
      end,
      fun(SavedModel) ->
          {SavedModel:some_integer() =:= 42,
            "Integer field not saved correctly"}
      end,
      fun(SavedModel) ->
          {SavedModel:some_float() =:= 3.14159,
            "Float field not saved correctly"}
      end
    ], 
    [ "Check for record in the database",
      fun(SavedModel) ->
          do(
            fun() ->
                boss_db:find(SavedModel:id())
            end,
            [
              fun(Result) ->
                  {Result:id() =:= SavedModel:id(),
                    "Returned record ID not the same as saved record ID"}
              end,
              fun(Result) ->
                  {Result:some_text() =:= SavedModel:some_text(),
                    "Returned record Text not the same as saved record Text"}
              end,
              fun(Result) ->
                  {Result:some_boolean() =:= SavedModel:some_boolean(),
                    "Returned record Boolean not the same as saved record Boolean"}
              end,
              fun(Result) ->
                  {Result:some_time() =:= SavedModel:some_time(),
                    "Returned record Time not the same as saved record Time"}
              end,
              fun(Result) ->
                  {Result:some_integer() =:= SavedModel:some_integer(),
                    "Returned record Integer not the same as saved record Integer"}
              end,
              fun(Result) ->
                  {Result:some_float() =:= SavedModel:some_float(),
                    "Returned record Float not the same as saved record Float"}
              end
            ], [])
      end,

      "Create more records",
      fun(Model1) ->
          do(
            fun() ->
                % boss_db_test_model:new(id, <<"Economists do it with models">>, 
                % {{1776, 7, 4}, {0, 0, 0}}, true, 42, 3.14159),
                {ok, Model2} = (boss_db_test_model:new(id, <<"Just do it.">>, 
                    {{1969, 7, 20}, {0, 0, 0}}, false, 100, 5.5)):save(),
                {ok, Model3} = (boss_db_test_model:new(id, <<"A Just Society">>, 
                    {{1984, 1, 1}, {0, 0, 0}}, true, 200, 28.8)):save(),
                [Model1:id(), Model2:id(), Model3:id()]
            end, 
            [
              fun(_) ->
                  Res = boss_db:find(boss_db_test_model, [], 1),
                  {length(Res) =:= 1, "Max not obeyed"}
              end,
              fun(_) ->
                  Res = boss_db:find(boss_db_test_model, [], many, 1),
                  {length(Res) =:= 2, "Skip not obeyed"}
              end,
              fun([_Id1, _Id2, Id3]) ->
                  Res = boss_db:find(boss_db_test_model, [], 1, 0, some_text, str_ascending),
                  {(hd(Res)):id() =:= Id3, "Sort str_ascending failed"}
              end,
              fun([_Id1, Id2, _Id3]) ->
                  Res = boss_db:find(boss_db_test_model, [], 1, 0, some_text, str_descending),
                  {(hd(Res)):id() =:= Id2, "Sort str_descending failed"}
              end,
              fun([Id1, _Id2, _Id3]) ->
                  Res = boss_db:find(boss_db_test_model, [], 1, 0, some_integer, num_ascending),
                  {(hd(Res)):id() =:= Id1, "Sort num_ascending failed"}
              end,
              fun([_Id1, _Id2, Id3]) ->
                  Res = boss_db:find(boss_db_test_model, [], 1, 0, some_integer, num_descending),
                  {(hd(Res)):id() =:= Id3, "Sort num_descending failed"}
              end,

              fun(_) ->
                  {boss_db:count(boss_db_test_model) =:= 3, "Count failed"}
              end,
              fun(_) ->
                  {boss_db:count(boss_db_test_model, []) =:= 3, "Count with empty conditions failed"}
              end,
              fun(_) ->
                  {boss_db:count(boss_db_test_model, [{some_integer, 'gt', 50}]) =:= 2, 
                    "Count with non-trivial conditions failed"}
              end
            ], 
            [
              "Run query tests",
              fun(Ids) ->
                  do(fun() -> ok end, [], generate_query_tests(Ids))
              end,

              "Delete records",
              fun([Id1|_]) ->
                  do(fun() ->
                        boss_db:delete(Id1)
                    end, 
                    [
                      fun(RetVal) ->
                          {RetVal =:= ok, "Return value not OK"}
                      end,
                      fun(_RetVal) ->
                          {boss_db:count(boss_db_test_model) =:= 2,
                            "Incorrect # of records in the DB"}
                      end
                    ], [])
              end
            ]
          )
      end
    ]).

do(Fun, Assertions, Continuations) ->
  boss_test:process_assertions_and_continuations(Assertions, Continuations, Fun()).

query_tests([Id1, Id2, Id3]) ->
  [
    {[],                                                [Id1, Id2, Id3]},

    {[{some_integer, 'equals', 100}],                   [     Id2     ]},
    {[{some_integer, 'not_equals', 100}],               [Id1,      Id3]},
    {[{some_integer, 'gt', 100}],                       [          Id3]},
    {[{some_integer, 'ge', 100}],                       [     Id2, Id3]},
    {[{some_integer, 'lt', 100}],                       [Id1          ]},
    {[{some_integer, 'le', 100}],                       [Id1, Id2     ]},
    {[{some_integer, 'in', {99, 101}}],                 [     Id2     ]},
    {[{some_integer, 'not_in', {99, 101}}],             [Id1,      Id3]},
    {[{some_integer, 'in', [42, 200]}],                 [Id1,      Id3]},
    {[{some_integer, 'not_in', [42, 200]}],             [     Id2     ]},

    {[{some_float, 'equals', 5.5}],                     [     Id2     ]},
    {[{some_float, 'not_equals', 5.5}],                 [Id1,      Id3]},
    {[{some_float, 'gt', 5.5}],                         [          Id3]},
    {[{some_float, 'ge', 5.5}],                         [     Id2, Id3]},
    {[{some_float, 'lt', 5.5}],                         [Id1          ]},
    {[{some_float, 'le', 5.5}],                         [Id1, Id2     ]},
    {[{some_float, 'in', {20, 30}}],                    [          Id3]},
    {[{some_float, 'not_in', {20, 30}}],                [Id1, Id2     ]},
    {[{some_float, 'in', [5.5, 28.8]}],                 [     Id2, Id3]},
    {[{some_float, 'not_in', [5.5, 28.8]}],             [Id1          ]},

    {[{some_boolean, 'equals', true}],                  [Id1,      Id3]},
    {[{some_boolean, 'not_equals', true}],              [     Id2     ]},

    {[{some_time, 'equals', {{1969, 7, 20}, {0,0,0}}}], [     Id2     ]},
    {[{some_time, 'not_equals',{{1969,7,20},{0,0,0}}}], [Id1,      Id3]},
    {[{some_time, 'gt', {{1969, 7, 20}, {0, 0, 0}}}],   [          Id3]},
    {[{some_time, 'ge', {{1969, 7, 20}, {0, 0, 0}}}],   [     Id2, Id3]},
    {[{some_time, 'lt', {{1969, 7, 20}, {0, 0, 0}}}],   [Id1          ]},
    {[{some_time, 'le', {{1969, 7, 20}, {0, 0, 0}}}],   [Id1, Id2     ]},

    {[{some_text, 'equals', "Just do it."}],            [     Id2     ]},
    {[{some_text, 'not_equals', "Just do it."}],        [Id1,      Id3]},
    {[{some_text, 'matches', "do it[\\. ]"}],           [Id1, Id2     ]},
    {[{some_text, 'not_matches', "do it[\\. ]"}],       [          Id3]},
    {[{some_text, 'contains', "Just"}],                 [     Id2, Id3]},
    {[{some_text, 'not_contains', "Just"}],             [Id1          ]},
    {[{some_text, 'contains_all', ["Just", "A"]}],      [          Id3]},
    {[{some_text, 'not_contains_all', ["Just", "A"]}],  [Id1, Id2     ]},
    {[{some_text, 'contains_any', ["with", "A"]}],      [Id1,      Id3]},
    {[{some_text, 'contains_none', ["with", "A"]}],     [     Id2     ]}

  ].

generate_query_tests(Ids) ->
  lists:foldr(fun({Conditions, ModelIds}, Acc) ->
        [io_lib:format("Find: ~p", [Conditions]),
          fun(_) ->
              do(
                fun() ->
                    Results = boss_db:find(boss_db_test_model, Conditions),
                    lists:map(fun(Res) -> Res:id() end, Results)
                end,
                [
                  fun(ResultIds) ->
                      {length(ResultIds) =:= length(ModelIds), "Query returned incorrect number of records"}
                  end,
                  fun(ResultIds) ->
                      {lists:sort(ResultIds) =:= lists:sort(ModelIds), "Query returned an incorrect set"}
                  end
                ], [])
          end | Acc]
    end, [], query_tests(Ids)).
