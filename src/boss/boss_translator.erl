%% @doc Chicago Boss translator service

-module(boss_translator).

-export([start/0, start/1, stop/0]).

-export([
        is_loaded/2,
        lookup/3, 
        fun_for/2,
        reload/2,
        reload_all/1
    ]).

start() ->
    start([]).

start(Options) ->
    boss_translator_sup:start_link(Options).

stop() ->
    ok.

%% @spec lookup(Key::string(), Locale::string()) -> Translation::string() | undefined
lookup(Pid, Key, Locale) ->
    gen_server:call(Pid, {lookup, Key, Locale}).

%% @spec is_loaded(Locale::string()) -> true | false
is_loaded(Pid, Locale) ->
    gen_server:call(Pid, {is_loaded, Locale}).

%% @spec reload(Locale::string()) -> ok | {error, Reason}
reload(Pid, Locale) ->
    gen_server:call(Pid, {reload, Locale}).

%% @spec reload_all() -> ok | {error, Reason}
reload_all(Pid) ->
    gen_server:call(Pid, reload_all).

%% @spec fun_for(Locale::string()) -> TranslationFun::function() | none
fun_for(Pid, Locale) ->
    case is_loaded(Pid, Locale) of
        true -> fun(String) -> ?MODULE:lookup(Pid, String, Locale) end;
        false -> none
    end.
