-module(boss_session_adapter_ets).
-behaviour(boss_session_adapter).
-export([start/0, start/1, stop/1]).
-export([new_session/2, get_session_data/2, set_session_data/4]).
-export([delete_session/2, remove_session_data/3]).

-record(boss_session, {sid,data,ttl}).

start() ->
    start([]).

start(_Options) ->
    ets:new(?MODULE,[set,named_table,{keypos, 2}]),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    {ok, undefined}.

stop(_) ->
	ok.

new_session(_, Cookie) ->	
    case Cookie of
		undefined ->
		    make_session();
		Any ->
		    case ets:member(?MODULE, Any) of
			true -> Any;
			false -> make_session()
		    end
	end.

get_session_data(_, Sid) ->
    case ets:lookup(?MODULE, Sid) of
	       [S] -> S#boss_session.data;
	       [] -> []
	end.	

set_session_data(_, Sid, Key, Value) ->
	Data = case ets:lookup(?MODULE,Sid) of
	       [S] ->
		   S#boss_session.data;
	       [] -> []
	   end,
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    Rest = proplists:delete(Key,Data),
		    [{Key,Value}|Rest];
		false ->
		    [{Key,Value}|Data]
	    end,
    
    ets:insert(?MODULE,#boss_session{sid=Sid,data=Data1,ttl=0}),
    ok.

delete_session(_, Sid) ->
	ets:delete(?MODULE,Sid),
	ok.

remove_session_data(_, Sid, Key) ->
    Data = case ets:lookup(?MODULE,Sid) of
	       [S] ->
		   S#boss_session.data;
	       [] -> []
	   end,
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    proplists:delete(Key,Data);
		false ->
		    Data
	    end,
    
    ets:insert(?MODULE,#boss_session{sid=Sid,data=Data1,ttl=0}),
	ok.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

make_session() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:sha(Data)),
    Id = lists:flatten(list_to_hex(Sha_list)),
    Session = #boss_session{sid=Id,data=[],ttl=0},
    ets:insert(?MODULE,Session),
    Id.

%% Convert Integer from the SHA to Hex
list_to_hex(L)->
       lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 -> 
       [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
