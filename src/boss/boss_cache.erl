-module(boss_cache).
-export([start/0, start/1]).
-export([stop/0]).
-export([get/1, set/3, handle_record_news/3, handle_collection_news/3, delete/1]).

handle_record_news(_, _, Key) ->
    delete(Key),
    {ok, cancel_watch}.

handle_collection_news(deleted, _, Key) ->
    delete(Key),
    {ok, cancel_watch};
handle_collection_news(created, _, Key) ->
    delete(Key),
    {ok, cancel_watch};
handle_collection_news(updated, {_Record, Attr, _OldVal, _NewVal}, Key) ->
    Conditions = element(3, Key),
    case proplists:lookup(Attr, Conditions) of
        none ->
            ok;
        _ ->
            delete(Key),
            {ok, cancel_watch}
    end.

start() ->
    CacheAdapter = boss_env:get_env(cache_adapter, memcached_bin),
    start([{adapter, list_to_atom("boss_cache_adapter_"++atom_to_list(CacheAdapter))},
            {cache_servers, boss_env:get_env(cache_servers, [{"127.0.0.1", 11211, 1}])}]).

start(Options) ->
    boss_cache_sup:start_link(Options).

stop() ->
    ok.

set(Key, Val, TTL) ->
    io:format("Setting cache for key ~p~n", [Key]),
    gen_server:call(?MODULE, {set, Key, Val, TTL}).

get(Key) ->
    io:format("Getting cache for key ~p~n", [Key]),
    gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    io:format("Deleting cache for key ~p~n", [Key]),
    gen_server:call(?MODULE, {delete, Key}).
