-module(boss_session_test_app).

-behaviour(application).
-export([start/2, stop/1]).


-spec(start(_,_) -> no_return()).
start(_Type, _StartArgs) ->
    CacheAdapter = boss_env:cache_adapter(),
    CacheOptions =
        case CacheAdapter of
            ets ->
                MaxSize = boss_env:get_env(ets_maxsize, 32 * 1024 * 1024),
                Threshold = boss_env:get_env(ets_threshold, 0.85),
                Weight = boss_env:get_env(ets_weight, 30),
                [{adapter, ets}, {ets_maxsize, MaxSize},
                 {ets_threshold, Threshold}, {ets_weight, Weight}];
            _ ->
                [{adapter, CacheAdapter},
                 {cache_servers, boss_env:get_env(cache_servers, [{"127.0.0.1", 11211, 1}])}]
        end,
    case application:get_env(session_adapter) of
        {ok, mnesia} ->
            mnesia:stop(),
            mnesia:create_schema([node()]);		  
        {ok, cache} ->
            boss_cache:start(CacheOptions);
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
        SessionID = "Foobar",
	do(
      fun() ->
		boss_session:set_session_data(SessionID, Key1, Key1Value),
		boss_session:set_session_data(SessionID, Key2, Key2Value)
      end,
	  [
	   	fun(_) ->
			{boss_session:get_session_data(SessionID, Key1) =:= Key1Value,
			 "Key1 value not stored correctly in session"}	
		end,
	   	fun(_) ->
			{boss_session:get_session_data(SessionID, Key2) =:= Key2Value,
			 "Key2 value not stored correctly in session"}	
		end		
	   ],
	   [ "Get all data from session",
		 fun(_) ->
			do(
			  fun() ->
			  	Result = boss_session:get_session_data(SessionID),
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
		 fun(_) ->
			do(
			  fun() ->
			  	Key1Result = boss_session:get_session_data(SessionID, Key1),
				Key2Result = boss_session:get_session_data(SessionID, Key2),
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
		 fun(_) ->
			do(
			  fun() ->
			  	boss_session:remove_session_data(SessionID, Key1),
				Key1Result = boss_session:get_session_data(SessionID, Key1),
				Key2Result = boss_session:get_session_data(SessionID, Key2),
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
		 fun(_) ->
			do(
			  fun() ->
			  	boss_session:delete_session(SessionID),
				AllResult = boss_session:get_session_data(SessionID),
				Key1Result = boss_session:get_session_data(SessionID, Key1),
				Key2Result = boss_session:get_session_data(SessionID, Key2),
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

