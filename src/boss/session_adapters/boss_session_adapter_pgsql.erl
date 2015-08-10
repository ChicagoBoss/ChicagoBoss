-module(boss_session_adapter_pgsql).
-behaviour(boss_session_adapter).
-export([start/0, start/1, stop/1, init/1]).

-export([session_exists/2, create_session/3, lookup_session/2]).
-export([lookup_session_value/3, set_session_value/4, delete_session/2, delete_session_value/3]).

-record(conn, {
        prefix,
        exp_time
    }).

start() ->
    start([]).

start(_Options) ->
    {ok, #conn{ prefix = sess }}.

stop(_Conn) ->
    ok.

init(_Options) ->
    error_logger:info_msg("Starting distributed session Postgresql storage~n"),

    %% Do we have a session table?  If not, create it.
    case boss_db:table_exists(boss_session) of
        false ->
	    {ok, _, _} = boss_db:execute("CREATE TABLE boss_session (id VARCHAR NOT NULL PRIMARY KEY,"
					 " data HSTORE);");
        _ ->
            noop
    end.

session_exists(_, SessionID) ->
    {ok, _, Result} = boss_db:execute("SELECT * FROM boss_session WHERE id=$1;",[SessionID]),
    Result =/= [].

create_session(_, SessionID, []) ->
    {ok,_} = boss_db:execute("INSERT INTO boss_session (id) VALUES ($1);",[SessionID]),
%%% This never gets called with actual data.  If that changes, do something appropriate here
%%%    ok;
%%%create_session(_, SessionID, Data) ->
%%%    {ok,_,_} = boss_db:execute("INSERT INTO boss_session(id,data) VALUES ($1,$2);",[SessionID, encode(Data)]),
    ok.

lookup_session(_, SessionID) ->
    recover_data(SessionID).	

lookup_session_value(Conn, SessionID, Key) when is_atom(Key) ->
    lookup_session_value(Conn, SessionID, atom_to_list(Key));
lookup_session_value(_, SessionID, Key) when is_list(Key) ->
    Data = recover_data(SessionID),
    case proplists:get_value(Key, Data) of
	undefined -> [];
	V -> V
    end.

set_session_value(Conn, Sid, Key, Value) when is_atom(Key) ->
    set_session_value(Conn, Sid, atom_to_list(Key), Value);
set_session_value(Conn, Sid, Key, Value) when is_list(Key) ->
    F = fun(Data) ->
		Data1 = term_to_binary(Data),
		Data2 = base64:encode(Data1),
		Data3 = binary_to_list(Data2),
		Data3
	end,
    delete_session_value(Conn, Sid, Key),
    case lookup_session(Conn,Sid) of
	[] ->
	    {ok,_} = boss_db:execute("UPDATE boss_session SET data='"
			    ++ Key ++ "=>" ++ F(Value) ++ "'::hstore WHERE id=$1;", [Sid]);
	_ ->
	    {ok,_} = boss_db:execute("UPDATE boss_session SET data=data || '"
			    ++ Key ++ "=>" ++ F(Value) ++ "'::hstore WHERE id=$1;", [Sid])
    end,
    ok.

delete_session(_, Sid) ->
    {ok,_} = boss_db:execute("DELETE FROM boss_session WHERE id=$1;",[Sid]),
    ok.

delete_session_value(Conn, Sid, Key) when is_atom(Key) ->
    delete_session_value(Conn, Sid, atom_to_list(Key));
delete_session_value(Conn, Sid, Key) when is_list(Key) ->
    case lookup_session_value(Conn,Sid, Key) of
	undefined -> ok;
	[] -> ok;
	_ ->
	    {ok,_} = boss_db:execute("UPDATE boss_session SET data=delete(data,$1) where id=$2;",[Key,Sid]),
	    ok
    end.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
recover_data(Sid)->
    F = fun(X) -> [C || C <- binary_to_list(X), ((C =/= 32) and (C =/= $\\) and (C =/= $\"))] end,
    F2 = fun(Data) ->
                Data1 = [C || C <- binary_to_list(Data), (C =/= $\")],
		Data2 = base64:decode(Data1),
		Data3 = binary_to_term(Data2),
		Data3
	end,
    case boss_db:execute("SELECT data FROM boss_session WHERE id=$1;",[Sid]) of
        {ok,_,[{null}]} -> [];
        {ok,_,[{<<>>}]} -> [];
	{ok,_,[{Data}]} ->
	    Data1 = binary:split(Data, <<",">>),
	    Data2 = lists:map(fun(X) -> binary:split(X,<<"=>">>) end, Data1),
	    Data3 = lists:map(fun([X,Y]) -> {F(X),F2(Y)} end, Data2),
            Data3;
	_ -> []
    end.

