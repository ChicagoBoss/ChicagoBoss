-module(boss_session_adapter_cache).
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

init(_) ->
    ok.

session_exists(_, undefined) ->
    false;
session_exists(#conn{ prefix = Prefix }, SessionID) ->
    boss_cache:get(Prefix, SessionID) =/= undefined.

create_session(#conn{ prefix = Prefix }, SessionID, InitialData) ->
    boss_cache:set(Prefix, SessionID, InitialData, 0).

lookup_session(#conn{ prefix = Prefix}, SessionID) ->
    case boss_cache:get(Prefix, SessionID) of
        undefined -> [];
        Val -> Val
    end.

lookup_session_value(#conn{ prefix = Prefix }, SessionID, Key) ->
    case boss_cache:get(Prefix, SessionID) of
        undefined -> undefined;
        PropList -> proplists:get_value(Key, PropList)
    end.

set_session_value(#conn{ prefix = Prefix }, SessionID, Key, Value) ->
    case boss_cache:get(Prefix, SessionID) of
        undefined -> 
            create_session(#conn{ prefix = Prefix }, SessionID, [{Key, Value}]);
        PropList ->
            boss_cache:set(Prefix, SessionID, [{Key, Value}|proplists:delete(Key, PropList)], 0)
    end.

delete_session(#conn{ prefix = Prefix }, SessionID) ->
    boss_cache:delete(Prefix, SessionID).

delete_session_value(#conn{ prefix = Prefix }, SessionID, Key) ->
    case boss_cache:get(Prefix, SessionID) of
        undefined -> ok;
        PropList ->
            boss_cache:set(Prefix, SessionID, proplists:delete(Key, PropList), 0)
    end.
