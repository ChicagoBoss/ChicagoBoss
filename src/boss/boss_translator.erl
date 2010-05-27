%% @doc Chicago Boss translator service

-module(boss_translator).

-export([start/0, start/1, stop/0]).

-export([
        is_loaded/1,
        lookup/2, 
        reload/1
    ]).

start() ->
    start([]).

start(_Options) ->
    application:start(boss_translator).

stop() ->
    application:stop(boss_translator).

%% @spec lookup(Key::string(), Locale::string()) -> Translation::string() | undefined
lookup(Key, Locale) ->
    gen_server:call(boss_translator, {lookup, Key, Locale}).

%% @spec is_loaded(Locale::string()) -> true | false
is_loaded(Locale) ->
    gen_server:call(boss_translator, {is_loaded, Locale}).

%% @spec reload(Locale::string()) -> ok | {error, Reason}
reload(Locale) ->
    gen_server:call(boss_translator, {reload, Locale}).
