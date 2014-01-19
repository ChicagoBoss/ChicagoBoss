-module(boss_controller_compiler_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").
-define(TMODULE, boss_controller_compiler).


spec_test_() ->
     begin
         Tests = [
                  {extract_routes_from_clauses, 2},
                  {function_for_routes, 1},
                  {route_from_token_ast, 1},
                  {map_syntax_tuples,1},
                  {map_tokens, 1}
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
