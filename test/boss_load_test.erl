-module(boss_load_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").



load_view_inner_test() ->
    Inner	= boss_load:load_views_inner(test, ".", self()),
    ?assert(is_function(Inner, 2)),
    [DtlModule] = Inner("../test/good.dtl", []),
    ?assertEqual(test_test_good_dtl, DtlModule),
    Exports     = DtlModule:module_info(exports),
    ?assertEqual([0,1,2], proplists:get_all_values(render, Exports)).

load_view_inner_bad_test() ->
    Inner	= boss_load:load_views_inner(test, ".", self()),
    ?assert(is_function(Inner, 2)),
    ?assertEqual([],Inner("../test/bad.dtl", [])).
   

load_view_inner_no_file_test() ->
    Inner	= boss_load:load_views_inner(test, ".", self()),
    ?assert(is_function(Inner, 2)),
    ?assertEqual([test], Inner("../test/no_file.dtl", [test])).



module_is_loaded_test() ->
    ?assert(boss_load:module_is_loaded('boss_load')),
    ?assertNot(boss_load:module_is_loaded('boss_load1')),
    ?assert(proper:check_spec({boss_load, module_is_loaded, 1},
                              
                              [{to_file, user}])),
    ok.


make_computed_vsn_test() ->
    ?assertEqual("ok", boss_load:make_computed_vsn("echo ok")),
    ?assert(proper:quickcheck(prop_make_computed_vsn(),
                               [{to_file, user}])),
    ok.


prop_make_computed_vsn() ->
    ?FORALL(X,
            any(),
            X =:= boss_load:make_computed_vsn({unknown, X})).


make_all_modules_test() ->
    ?assertEqual([], boss_load:make_all_modules(test, "/tmp", [])),
    ?assert(proper:quickcheck(prop_make_all_modules(),
                               [{to_file, user}])),
    ?assert(proper:quickcheck(prop_make_all_modules_error(),
                               [{to_file, user}])),
    
    ok.

-type op_key()   :: test_modules|lib_modules|websocket_modules|mail_modules|controller_modules|
                    model_modules| view_lib_tags_modules|view_lib_helper_modules|view_modules.
-type op_list_el():: {op_key(), [atom()]}.
prop_make_all_modules()->
    ?FORALL( 
       OpKeys,
       [op_list_el()],
       begin
           Application = test,
           OutDir = "/tmp",
           Ops = [{Op, fun(IApp,IOutDir) ->
                               IApp    = Application,
                               IOutDir = OutDir,
                               {ok, Modules}
                            end}|| {Op, Modules} <- OpKeys],
           
           Result = boss_load:make_all_modules(Application, OutDir, Ops),

           Result -- OpKeys  =:= [] 
       end).

prop_make_all_modules_error()->
    ?FORALL( 
       OpKeys,
       [op_list_el()],
       begin
           Application = test,
           OutDir = "/tmp",
           Ops = [{Op, fun(IApp,IOutDir) ->
                               IApp    = Application,
                               IOutDir = OutDir,
                               {error, "test"}
                            end}|| {Op, _Modules} <- OpKeys],
           
           Result = boss_load:make_all_modules(Application, OutDir, Ops),
           [] =:= lists:concat([Value|| {_, Value} <-Result])

       end).

make_ops_list_test() ->
    OpsList = boss_load:make_ops_list(self()),
    ?assert(lists:all(fun({Op, Fun}) ->
                   is_atom(Op) andalso is_function(Fun,2)
           end, OpsList)),
    ok.
