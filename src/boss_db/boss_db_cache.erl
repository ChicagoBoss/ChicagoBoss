-module(boss_db_cache).
-export([handle_record_news/3, handle_collection_news/3]).

handle_record_news(_, _, {Prefix, Key}) ->
    boss_cache:delete(Prefix, Key),
    {ok, cancel_watch}.

handle_collection_news(deleted, _, {Prefix, Key}) ->
    boss_cache:delete(Prefix, Key),
    {ok, cancel_watch};
handle_collection_news(created, _, {Prefix, Key}) ->
    boss_cache:delete(Prefix, Key),
    {ok, cancel_watch};
handle_collection_news(updated, {_Record, Attr, _OldVal, _NewVal}, {Prefix, Key}) when Attr =:= element(5, Key) ->
    boss_cache:delete(Prefix, Key),
    {ok, cancel_watch};
handle_collection_news(updated, {_Record, Attr, _OldVal, _NewVal}, {Prefix, Key}) ->
    Conditions = element(2, Key),
    case proplists:lookup(Attr, Conditions) of
        none ->
            ok;
        _ ->
            boss_cache:delete(Prefix, Key),
            {ok, cancel_watch}
    end.

