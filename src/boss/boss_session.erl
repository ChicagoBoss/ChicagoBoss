%% @doc Chicago Boss session handler abstraction

-module(boss_session).

-export([start/0, start/1, stop/0]).
-export([get_session_key/0, get_session_exp_time/0]).
-export([new_session/1, get_session_data/1, get_session_data/2, set_session_data/3]).
-export([remove_session_data/2, delete_session/1]).

-define(POOLNAME, boss_session_pool).

start() ->
    SessionOptions  = make_session_options(),
    SessionDriver   = boss_env:session_adapter(),
    Adapter         = list_to_atom(lists:concat(["boss_session_adapter_", SessionDriver])),
    Adapter:init([]),
    SessionOptions1 = [{adapter, Adapter}|SessionOptions],
    start(SessionOptions1).

make_session_options() ->
    lists:foldl(fun(OptName, Acc) ->
                       case application:get_env(OptName) of
                           {ok, Val} -> [{OptName, Val}|Acc];
                           _ -> Acc
                       end
                end, [], [session_key, session_exp_time]).

start(Options) ->
    boss_session_sup:start_link(Options).

stop() ->
    ok.

get_session_key() ->
    boss_env:get_env(session_key, "_boss_session").

get_session_exp_time() ->
    boss_env:get_env(session_exp_time, 1440).

%% @spec new_session(Cookie::string()) -> string | {error, Reason}
%% @doc Starts new session with the specified `Cookie'.
new_session(Cookie) ->
    boss_pool:call(?POOLNAME, {new_session, Cookie}).

%% @spec get_session_data(SessionID) -> list | {error, Reason}
%% @doc Get session data for the `SessionID'.
get_session_data(SessionID) ->
    boss_pool:call(?POOLNAME, {get_session_data, SessionID}).

%% @spec get_session_data(SessionID, Key) -> list | {error, Reason}
%% @doc Get session data for the `SessionID' for a given `Key'.
get_session_data(SessionID, Key) ->
    boss_pool:call(?POOLNAME, {get_session_data, SessionID, Key}).

%% @spec set_session_data(SessionID, Key, Value) -> ok | {error, Reason}
%% @doc Set session data for the `SessionID'.
set_session_data(SessionID, Key, Value) ->
    boss_pool:call(?POOLNAME, {set_session_data, SessionID, Key, Value}).

%% @spec delete_session(SessionID) -> ok | {error, Reason}
%% @doc Delete session for given `SessionID'.
delete_session(SessionID) ->
    boss_pool:call(?POOLNAME, {delete_session, SessionID}).

%% @spec remove_session_data(SessionID, Key) -> ok | {error, Reason}
%% @doc Remove the Key from session data for the `SessionID'.
remove_session_data(SessionID, Key) ->
    boss_pool:call(?POOLNAME, {remove_session_data, SessionID, Key}).
