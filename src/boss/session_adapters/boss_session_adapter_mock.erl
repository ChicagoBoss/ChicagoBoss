-module(boss_session_adapter_mock).
-behaviour(boss_session_adapter).
-export([start/0, start/1, stop/1, init/1]).
-export([session_exists/2, create_session/3, lookup_session/2]).
-export([lookup_session_value/3, set_session_value/4, delete_session/2, delete_session_value/3]).

start() ->
    start([]).

start(_Options) ->
    {ok, undefined}.

stop(undefined) ->
    ok;
stop(MockSup) ->
    exit(MockSup),
    ok.

init(Options) ->
    case boss_env:is_master_node() of
        true ->
            boss_session_mock_sup:start_link(Options);
        false ->
            ok
    end.

session_exists(_, undefined) ->
    false;
session_exists(_, SessionID) ->
    gen_server:call({global, boss_session_mock}, {session_exists, SessionID}).

create_session(_, SessionID, Data) ->
    gen_server:call({global, boss_session_mock}, {create_session, SessionID, Data}).

lookup_session(_, SessionID) ->
    gen_server:call({global, boss_session_mock}, {lookup_session, SessionID}).

lookup_session_value(_, SessionID, Key) ->
    gen_server:call({global, boss_session_mock}, {lookup_session_value, SessionID, Key}).

set_session_value(_, SessionID, Key, Value) ->
    gen_server:call({global, boss_session_mock}, {set_session_value, SessionID, Key, Value}).

delete_session(_, SessionID) ->
    gen_server:call({global, boss_session_mock}, {delete_session, SessionID}).

delete_session_value(_, SessionID, Key) ->
    gen_server:call({global, boss_session_mock}, {delete_session_value, SessionID, Key}).
