-module(boss_session_adapter_mnesia).
-behaviour(boss_session_adapter).
-export([start/0, start/1, stop/1]).
-export([new_session/2, get_session_data/2, set_session_data/4]).
-export([delete_session/2, remove_session_data/3]).

-define(TABLE, boss_session).
-define(TIMEOUT, 20).

-record(boss_session, {sid,data,ttl}).

start() ->
    start([]).

start(_Options) ->
	error_logger:info_msg("Starting distributed session mnesia storage~n"),	
	mnesia:start(),
	%%Checks for table, after some time tries to recreate it
	case mnesia:wait_for_tables([?TABLE], ?TIMEOUT) of
		ok -> 
			error_logger:info_msg("mnesia session table ok~n"),	
			noop;
		{timeout,[?TABLE]} ->
			create_session_storage()		
	end,

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
			Read = fun() -> mnesia:read({?TABLE,Any}) end, 
		    case mnesia:transaction(Read) of
				{atomic, []} -> make_session();
				{atomic, _Element} -> Any 
		    end
	    end.

get_session_data(_, Sid) ->
    recover_data(Sid).	

set_session_data(_, Sid, Key, Value) ->
	Data = recover_data(Sid), 
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    Rest = proplists:delete(Key,Data),
		    [{Key,Value}|Rest];
		false ->
		    [{Key,Value}|Data]
	    end,
    
	Update = fun() -> NewSession = #boss_session{sid=Sid,data=Data1,ttl=0}, mnesia:write(NewSession) end,
    {atomic,ok} = mnesia:transaction(Update),	
    ok.

delete_session(_, Sid) ->
	Remove = fun() -> mnesia:delete({?TABLE, Sid}) end,
    mnesia:transaction(Remove),
	ok.

remove_session_data(_, Sid, Key) ->
    Data = recover_data(Sid),
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    proplists:delete(Key,Data);
		false ->
		    Data
	    end,
    Update = fun() -> NewSession = #boss_session{sid=Sid,data=Data1,ttl=0}, mnesia:write(NewSession) end,
    {atomic,ok} = mnesia:transaction(Update),
	ok.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
create_session_storage()->
	Nodes = mnesia_nodes(),
	error_logger:info_msg("Creating mnesia table for nodes ~p~n",  Nodes),
	case mnesia:create_table(?TABLE,[{disc_copies,  Nodes}, {attributes, record_info(fields, boss_session)}]) of
		{aborted, Reason} -> error_logger:error_msg("Error creating mnesia table for sessions: ~p~n", Reason);
		{atomic, ok} -> ok
	end.

recover_data(Sid)->
	Recover = fun() -> mnesia:read({?TABLE,Sid}) end,
    case mnesia:transaction(Recover) of
		{atomic, [S]} -> S#boss_session.data;
	    {atomic, []} -> []
	end.

mnesia_nodes()->
    case application:get_env(session_mnesia_nodes) of
        {ok, Val} -> Val;
        _ -> [node()]
    end.

make_session() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:sha(Data)),
    Id = lists:flatten(list_to_hex(Sha_list)),
    Session = #boss_session{sid=Id,data=[],ttl=0},
    Insert = fun() -> mnesia:write(Session) end,
	case mnesia:transaction(Insert) of
		{aborted, Reason} -> error_logger:error_msg("Error inserting session data in mnesia: ~p~n", Reason);
		{atomic, ok} -> ok
	end,
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
