-module(boss_cache).
-export([start/0, start/1]).
-export([stop/0]).
-export([get/2, set/4, delete/2]).

start() ->
    start([{adapter, boss_cache_adapter_memcached_bin}, {cache_servers, [{"127.0.0.1", 11211, 1}]}]).

start(Options) ->
    boss_cache_sup:start_link(Options).

stop() ->
    ok.

set(Prefix, Key, Val, TTL) ->
    gen_server:call(?MODULE, {set, Prefix, Key, Val, TTL}).

get(Prefix, Key) ->
    gen_server:call(?MODULE, {get, Prefix, Key}).

delete(Prefix, Key) ->
    gen_server:call(?MODULE, {delete, Prefix, Key}).
