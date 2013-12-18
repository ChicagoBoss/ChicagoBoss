-module(boss_load_test).
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



