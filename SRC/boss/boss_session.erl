%% @doc Chicago Boss session handler abstraction

-module(boss_session).

-export([start/0, start/1, stop/0]).
-export([get_session_key/0, get_session_exp_time/0]).
-export([new_session/1, get_session_data/1, get_session_data/2, set_session_data/3]).
-export([remove_session_data/2, delete_session/1]).

start() ->
    SessionOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [session_key]),
    SessionDriver = case application:get_env(session_adapter) of
        {ok, Val} -> Val;
        _ -> ets
    end,
    SessionOptions1 = [{adapter, list_to_atom("boss_session_adapter_"++atom_to_list(SessionDriver))}|SessionOptions],
    start(SessionOptions1).

start(Options) ->
    boss_session_sup:start_link(Options).

stop() ->
    ok.

get_session_key() ->
    case application:get_env(session_key) of
        {ok, Val} -> Val;
        _ -> "_boss_session"
    end.

get_session_exp_time() ->
	case application:get_env(session_exp_time) of
        {ok, EVal} -> EVal;
        _ -> 1440
    end.

get_sid_from_request(Req) ->
	 Req:cookie(boss_session:get_session_key()).

%% @spec new_session(Cookie::string()) -> string | {error, Reason}
%% @doc Starts new session with the specified Cookie.
new_session(Cookie) ->
    gen_server:call(boss_session, {new_session, Cookie}).

%% @spec get_session_data(Req) -> list | {error, Reason}
%% @doc Get session data for the Request.
get_session_data(Req) ->
    Sid = get_sid_from_request(Req),
	gen_server:call(?MODULE,{get_session_data, Sid}).

%% @spec get_session_data(Req, Key) -> list | {error, Reason}
%% @doc Get session data for the Request for a given Key.
get_session_data(Req, Key) ->
    Sid = get_sid_from_request(Req),
	SessionData = gen_server:call(?MODULE,{get_session_data, Sid}),
    proplists:get_value(Key, SessionData).

%% @spec set_session_data(Req,Key,Value) -> ok | {error, Reason}
%% @doc Set session data for the Sid.
set_session_data(Req, Key, Value) ->
	Sid = get_sid_from_request(Req),
    gen_server:call(?MODULE,{set_session_data, Sid, Key, Value}).

%% @spec delete_session(Req) -> ok | {error, Reason}
%% @doc Delete session for given Sid.
delete_session(Req) ->
    Sid = get_sid_from_request(Req),	
    gen_server:call(?MODULE,{delete_session, Sid}).

%% @spec remove_session_data(Sid, Key) -> ok | {error, Reason}
%% @doc Remove the Key from session data for the Sid.
remove_session_data(Req, Key) ->
    Sid = get_sid_from_request(Req),	
    gen_server:call(?MODULE,{remove_session_data, Sid, Key}).
