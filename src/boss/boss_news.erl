-module(boss_news).

-export([start/0, start/1]).

-export([stop/0]).

-export([watch/2, watch/3, set_watch/3, set_watch/4]).

-export([cancel_watch/1, extend_watch/1]).

-export([deleted/2, updated/3, created/2]).

-export([reset/0, dump/0]).

-define(TRILLION, 1000 * 1000 * 1000 * 1000).

start() ->
    start([]).

start(Options) ->
    boss_news_sup:start_link(Options),
    case boss_load:module_is_loaded(news) of
        true -> news:init();
        false -> ok
    end.

stop() ->
    ok.

%% @doc Watch a record's attributes, and execute `CallBack' when they change.
%% @spec watch( TopicString :: string(), CallBack ) -> {ok, WatchId} | {error, Reason}
watch(TopicString, CallBack) ->
    watch(TopicString, CallBack, ?TRILLION).

watch(TopicString, CallBack, TTL) ->
    gen_server:call(boss_news, {watch, TopicString, CallBack, TTL}).

set_watch(WatchId, TopicString, CallBack) ->
    set_watch(WatchId, TopicString, CallBack, ?TRILLION).

set_watch(WatchId, TopicString, CallBack, TTL) ->
    gen_server:call(boss_news, {set_watch, WatchId, TopicString, CallBack, TTL}).

cancel_watch(WatchId) ->
    gen_server:call(boss_news, {cancel_watch, WatchId}).

extend_watch(WatchId) ->
    gen_server:call(boss_news, {extend_watch, WatchId}).

deleted(Id, Attrs) ->
    gen_server:call(boss_news, {deleted, Id, Attrs}).

updated(Id, OldAttrs, NewAttrs) ->
    gen_server:call(boss_news, {updated, Id, OldAttrs, NewAttrs}).

created(Id, NewAttrs) ->
    gen_server:call(boss_news, {created, Id, NewAttrs}).

reset() ->
    gen_server:call(boss_news, reset),
    case boss_load:module_is_loaded(news) of
        true -> news:init();
        false -> ok
    end.

dump() ->
    gen_server:call(boss_news, dump).
