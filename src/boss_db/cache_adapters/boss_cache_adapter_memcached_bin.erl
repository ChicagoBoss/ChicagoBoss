-module(boss_cache_adapter_memcached_bin).
-behaviour(boss_cache_adapter).

-export([start/0, start/1, stop/1]).
-export([get/3, set/5, delete/3]).

start() ->
    start([]).

start(Options) ->
    CacheServers = proplists:get_value(cache_servers, Options, [{"localhost", 11211, 1}]),
    ok = erlmc:start(CacheServers),
    {ok, undefined}.

stop(_Conn) ->
    erlmc:quit().

get(_Conn, Prefix, Key) ->
    case erlmc:get(term_to_key(Prefix, Key)) of
        <<>> -> 
            undefined;
        Bin -> 
            binary_to_term(Bin)
    end.

set(_Conn, Prefix, Key, Val, TTL) ->
    erlmc:set(term_to_key(Prefix, Key), term_to_binary(Val), TTL).

delete(_Conn, Prefix, Key) ->
    erlmc:delete(term_to_key(Prefix, Key)).

% internal
term_to_key(Prefix, Term) ->
    lists:concat([Prefix, ":", mochihex:to_hex(erlang:md5(term_to_binary(Term)))]).
