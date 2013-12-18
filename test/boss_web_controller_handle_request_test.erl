-module(boss_web_controller_handle_request_test).

-include_lib("eunit/include/eunit.hrl").



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


make_etag_test() ->
    Sha1 = boss_web_controller_handle_request:make_etag(boss, "", "empty"),
    ?assertEqual("2jmj7l5rSw0yVb/vlWAYkK/YBwk=", Sha1).
  
dev_headers_test() ->
    Module = {simple_bridge_response_wrapper,mochiweb_response_bridge,
	      {{mochicow_request,port,'GET',"/static/test.txt",
		{1,1},
		{3,
		 {"user-agent",
		  {"user-agent","curl/7.32.0"},
		  {"host",
		   {"host","erlang-ci.com:8001"},
		   {"accept",{"accept","*/*"},nil,nil},
		   nil},
		  nil}},
		<<>>},
	       "/home/zkessin/Documents/erlang_ci/priv/static"},
	      {response,200,[{header,"Cache-Control","no-cache"}],[],[]}},
    ?assertEqual(Module, boss_web_controller_handle_request:dev_headers(Module, production)),
    {_,_,_,Response} = boss_web_controller_handle_request:dev_headers(Module, development),
    ?assertMatch({response, 200, [{header, "Cache-Control","no-cache"}],_,_}, Response).


