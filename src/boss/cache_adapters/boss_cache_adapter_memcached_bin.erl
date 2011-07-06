-module(boss_cache_adapter_memcached_bin).
-behaviour(boss_cache_adapter).

-export([start/0, start/1, stop/1]).
-export([get/2, set/4, delete/2]).

start() ->
    start([]).

start(Options) ->
    CacheServers = proplists:get_value(cache_servers, Options, [{"localhost", 11211, 1}]),
    ok = erlmc:start(CacheServers),
    {ok, undefined}.

stop(_Conn) ->
    erlmc:quit().

get(_Conn, Key) ->
    case erlmc:get(term_to_key(Key)) of
        <<>> -> 
            undefined;
        Bin -> 
            binary_to_term(Bin)
    end.

set(_Conn, Key, Val, TTL) ->
    erlmc:set(term_to_key(Key), term_to_binary(Val), TTL).

delete(_Conn, Key) ->
    erlmc:delete(term_to_key(Key)).

term_to_key(Term) ->
    mochihex:to_hex(erlang:md5(term_to_binary(Term))).
