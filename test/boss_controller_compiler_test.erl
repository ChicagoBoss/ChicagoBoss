-module(boss_controller_compiler_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").
-define(TMODULE, boss_controller_compiler).
-compile(export_all).

spec_test_() ->
     begin
         Tests = [
                  fun prop_add_routes_to_forms/0,

                  {add_export_to_forms,         1},
                  {extract_routes_from_clauses, 2},
                  {function_for_routes,         1},
                  {route_from_token_ast,        1},
                  {map_syntax_tuples,           1},
                  {map_tokens,                  1}
                 ],
         [case Test of
             {Funct, Arity} ->

                  ?_assert(proper:check_spec({?TMODULE, Funct, Arity},
                                            [{to_file, user}, 25]));
                  
              F when is_function(F,0) ->
                  ?_assert(proper:quickcheck(F(), 
                                             [{to_file, user}]))
          end||Test <-Tests]
     end.


%-type route_form()       :: boss_controller_compiler:route_form().
prop_add_routes_to_forms() ->
    ?FORALL(ExportAttrs ,
            list(boss_controller_compiler:export_attr1()),
            ?IMPLIES(len_in_range(ExportAttrs,1,20),
                     begin
                         Result = boss_controller_compiler:add_routes_to_forms(ExportAttrs
                                                                               ++ [{eof, 1}]),
                         is_list(Result)
                     end)).


prop_add_routes_to_forms_function() ->
    ?FORALL(FctTups,
            list(route_form()),
            ?IMPLIES(len_in_range(FctTups, 1,5),
                     ?TIMEOUT(300, 
                              begin
                                  Result = boss_controller_compiler:add_routes_to_forms(FctTups,[],[]),
                                  is_list(Result)
                              end))).


len_in_range(List, Min, Max) ->
    Len = length(List),
    Len =< Max andalso Len >= Min.
