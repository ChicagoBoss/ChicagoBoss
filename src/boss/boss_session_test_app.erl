-module(boss_session_test_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  boss_db:start(),
  case application:get_env(session_adapter) of
  	{ok, mnesia} ->
		  mnesia:stop(),
		  mnesia:create_schema([node()]);		  
    _ -> ok
  end,
  boss_session:start(),
  run_tests(),
  erlang:halt().

stop(_State) ->
  ok.

run_tests() ->
	io:format("~-60s", ["Inserting keys in session"]),
	Key1 = "ricky",
	Key1Value = "jarl",
	Key2 = "cleaf",
	Key2Value = "pascual",
	do(
      fun() ->
		Req = make_request('GET', '/', []),
		boss_session:set_session_data(Req, Key1, Key1Value),
		boss_session:set_session_data(Req, Key2, Key2Value),
		Req
      end,
	  [
	   	fun(Req) ->
			{boss_session:get_session_data(Req, Key1) =:= Key1Value,
			 "Key1 value not stored correctly in session"}	
		end,
	   	fun(Req) ->
			{boss_session:get_session_data(Req, Key2) =:= Key2Value,
			 "Key2 value not stored correctly in session"}	
		end		
	   ],
	   [ "Get all data from session",
		 fun(Req) ->
			do(
			  fun() ->
			  	Result = boss_session:get_session_data(Req),
				Result
			  end,
			  [
			   	fun(Result) ->
					{length(Result) == 2,
					 "Get session data not returning all values"}
				end,
			   	fun(Result) ->
					{proplists:get_value(Key1, Result) =:= Key1Value,
					 "Get session value for key1 not correctly"}
				end,
				fun(Result) ->
					{proplists:get_value(Key2, Result) =:= Key2Value,
					 "Get session value for key2 not correctly"}
				end
			   ], [])
		 end,
		 
	     "Get keys from session",
		 fun(Req) ->
			do(
			  fun() ->
			  	Key1Result = boss_session:get_session_data(Req, Key1),
				Key2Result = boss_session:get_session_data(Req, Key2),
				{Key1Result, Key2Result}
			  end,
			  [
			   	fun({Key1Result, _Key2Result}) ->
					{Key1Result =:= Key1Value,
					 "Get session value for key1 - direct - not correct"}
				end,
			   	fun({_Key1Result, Key2Result}) ->
					{Key2Result =:= Key2Value,
					 "Get session value for key2 - direct - not correct"}
				end
			   ], [])
		 end,
		 
	     "Removing keys",
		 fun(Req) ->
			do(
			  fun() ->
			  	boss_session:remove_session_data(Req, Key1),
				Key1Result = boss_session:get_session_data(Req, Key1),
				Key2Result = boss_session:get_session_data(Req, Key2),
				{Key1Result, Key2Result}
			  end,
			  [
			   	fun({Key1Result, _Key2Result}) ->
					{Key1Result =:= undefined,
					 "Key1 not removed correctly"}
				end,
			   	fun({_Key1Result, Key2Result}) ->
					{Key2Result =:= Key2Value,
					 "Get session value for key2 - after remove key1 - not correct"}
				end
			   ], [])
		 end,
		 
	     "Removing all session",
		 fun(Req) ->
			do(
			  fun() ->
			  	boss_session:delete_session(Req),
				AllResult = boss_session:get_session_data(Req),
				Key1Result = boss_session:get_session_data(Req, Key1),
				Key2Result = boss_session:get_session_data(Req, Key2),
				{AllResult, Key1Result, Key2Result}
			  end,
			  [
			    fun({AllResult, _Key1Result, _Key2Result}) ->
					{AllResult =:= [],
					 "Session not deleted"}
				end,
			   	fun({_AllResult, Key1Result, _Key2Result}) ->
					{Key1Result =:= undefined,
					 "Key1 not removed correctly after session delete"}
				end,
			   	fun({_AllResult, _Key1Result, Key2Result}) ->
					{Key2Result =:= undefined,
					 "Key2 not removed correctly after session delete"}
				end
			   ], [])
		 end		 		 		 
		 
		]
	  ).

do(Fun, Assertions, Continuations) ->
  boss_test:process_assertions_and_continuations(Assertions, Continuations, Fun()).

make_request(Method, Uri, Headers) ->
    Req = mochiweb_request:new(
        false, %Socket
        Method, Uri, {1, 0}, mochiweb_headers:make(Headers)),
    DocRoot = "./static",
    simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}).

