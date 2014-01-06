-module(boss_files_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").

make_extentions_test() ->
    Extentions		= boss_files:make_extentions(),

    ?assert(is_list(Extentions)).

lookup_module_by_adapater_test() ->
    ?assertEqual([], boss_files:lookup_module_by_adapater([], [], [], undefined)),
    Modules = boss_files:lookup_module_by_adapater(test_app, "test_file", [], 
						   boss_compiler_adapter_erlang),
    ?assertEqual(["test_file"], Modules).


make_modules_test() ->
    _Dir	       = ".",
    _Application       = 'test',
    _ModuleAcc	       = [],
    _ExtentionPropList = boss_files:make_extentions(),
    ok.



find_file_4_test() ->
    ?assertEqual([test], boss_files:find_file([], undefined, [test], undefined)).
    
    
make_modules_itterator_test()->
    Extentions	= boss_files:make_extentions(),
    Itter	= boss_files:make_modules_itterator(Extentions, test),
    ?assert(is_function(Itter, 2)),
    ?assertEqual([test],		Itter(".test",    [test])),
    ?assertEqual([test],		Itter(".",        [test])),
    ?assertEqual(["test"],		Itter("test.erl", [])),
    ?assertEqual(["Elixir.Test.Test"],	Itter("test.ex",  [])),
    ?assertEqual(["test"],		Itter("test.lfe", [])),
    ?assertEqual([],			Itter("test",     [])).

    
