-module(boss_news).

-export([start/0, start/1]).

-export([stop/0]).

-export([watch/2, watch_set/2]).

-export([deleted/2, updated/3, created/2]).

-export([reset/0]).

start() ->
    start([]).

start(Options) ->
    boss_news_sup:start_link(Options),
    news:init().

stop() ->
    ok.

watch(TopicString, CallBack) ->
    gen_server:call(boss_news, {watch, TopicString, CallBack}).

watch_set(TopicString, CallBack) ->
    gen_server:call(boss_news, {watch_set, TopicString, CallBack}).

deleted(Id, Attrs) ->
    gen_server:call(boss_news, {deleted, Id, Attrs}).

updated(Id, OldAttrs, NewAttrs) ->
    gen_server:call(boss_news, {updated, Id, OldAttrs, NewAttrs}).

created(Id, NewAttrs) ->
    gen_server:call(boss_news, {created, Id, NewAttrs}).

reset() ->
    gen_server:call(boss_news, reset),
    news:init().
