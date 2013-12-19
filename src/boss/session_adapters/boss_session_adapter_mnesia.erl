-module(boss_session_adapter_mnesia).
-behaviour(boss_session_adapter).
-export([start/0, start/1, stop/1, init/1]).
-export([session_exists/2, create_session/3, lookup_session/2]).
-export([lookup_session_value/3, set_session_value/4, delete_session/2, delete_session_value/3]).

-define(TABLE, boss_session).
-define(TIMEOUT, 1000).

-record(boss_session, {sid,data,ttl}).

start() ->
    start([]).

start(_Options) ->
    {ok, undefined}.

stop(_) ->
    ok.

init(_) ->
    error_logger:info_msg("Starting distributed session mnesia storage~n"),
    ok = ensure_schema(),  % Expects Mnesia to be stopped
    mnesia:start(),
    %%Checks for table, after some time tries to recreate it
    case mnesia:wait_for_tables([?TABLE], ?TIMEOUT) of
        ok -> 
            error_logger:info_msg("mnesia session table ok~n"),	
            noop;
        {timeout,[?TABLE]} ->
            create_session_storage()		
    end,

    {ok, undefined}.

session_exists(_, SessionID) ->
    {atomic, Result} = mnesia:transaction(fun() -> mnesia:read({?TABLE, SessionID}) end),
    Result =/= [].

create_session(_, SessionID, Data) ->
    Session = #boss_session{sid=SessionID,data=Data,ttl=0},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Session) end),
    ok.

lookup_session(_, SessionID) ->
    recover_data(SessionID).	

lookup_session_value(_, SessionID, Key) ->
    Data = recover_data(SessionID),
    proplists:get_value(Key, Data).

set_session_value(_, Sid, Key, Value) ->
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
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete({?TABLE, Sid}) end),
    ok.

delete_session_value(_, Sid, Key) ->
    Data = recover_data(Sid),
    case proplists:is_defined(Key, Data) of
        true ->
            Data1 = proplists:delete(Key, Data),
            Update = fun() -> NewSession = #boss_session{sid=Sid,data=Data1,ttl=0}, mnesia:write(NewSession) end,
            {atomic,ok} = mnesia:transaction(Update),
            ok;
        false ->
            ok
    end.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
ensure_schema() ->
    Nodes = mnesia_nodes(),
    case mnesia:create_schema(Nodes) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok;
        Error -> Error
    end.

create_session_storage()->
    Nodes = mnesia_nodes(),
    error_logger:info_msg("Creating mnesia table for nodes ~p~n",  [Nodes]),
    case mnesia:create_table(?TABLE,[{disc_copies,  Nodes}, {attributes, record_info(fields, boss_session)}]) of
        {aborted, Reason} -> error_logger:error_msg("Error creating mnesia table for sessions: ~p~n", [Reason]);
        {atomic, ok} -> ok
    end.

recover_data(Sid)->
    Recover = fun() -> mnesia:read({?TABLE, Sid}) end, 
    case mnesia:transaction(Recover) of
        {atomic, [S]} -> S#boss_session.data;
        {atomic, []} -> []
    end.

mnesia_nodes()->
    case application:get_env(session_mnesia_nodes) of
        {ok, Val} -> Val;
        _ -> [node()]
    end.
