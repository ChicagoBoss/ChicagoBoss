-module(boss_web_controller_handle_request_test).

-include_lib("eunit/include/eunit.hrl").


load_and_execute_development_test() ->
    ?assert(true). 

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

f1(_) ->
    {ok, test}.
f2(_) ->
    {ok, test2}.
f3(_) ->
    {error, test3}.

fold_operations_test() ->
    ErrorList = [fun f1/1, fun f2/1, fun f3/1],
    OKList    = [fun f1/1, fun f2/1],
    ?assertEqual({error, test3}, boss_web_controller_handle_request:fold_operations(app,ErrorList)),
    ?assertEqual({ok, test2}, boss_web_controller_handle_request:fold_operations(app,OKList)),
    ErrLists = perms(ErrorList),
    lists:foreach(fun(List) ->
			  ?assertEqual({error, test3},
				       boss_web_controller_handle_request:fold_operations(app,List))
		  end, ErrLists),
    lists:foreach(fun(List) ->
			  ?assertMatch({ok, _},
				       boss_web_controller_handle_request:fold_operations(app,List))
		  end,perms(OKList)).

make_controlle_names_test() ->
    ?assertEqual(["foo","bar","baz"], 
		 boss_web_controller_handle_request:make_controller_names([foo,bar,baz])).

