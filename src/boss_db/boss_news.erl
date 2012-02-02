-module(boss_news).

-export([start/0, start/1]).

-export([stop/0]).

-export([watch/2, watch/3, watch/4]).

-export([set_watch/3, set_watch/4, set_watch/5]).

-export([cancel_watch/1, extend_watch/1]).

-export([deleted/2, updated/3, created/2]).

-export([reset/0, dump/0]).

-define(TRILLION, 1000 * 1000 * 1000 * 1000).

start() ->
    start([]).

start(Options) ->
    boss_news_sup:start_link(Options).

stop() ->
    ok.

%% @doc Watch records and attributes described by `TopicString', and execute 
%% `CallBack(Event, EventInfo)' each time any of them changes.
%% Note that the callback should be specified as a named fun and not a closure,
%% or you may experience unexpected results during code reloads.
%% @spec watch( TopicString :: string(), CallBack ) -> {ok, WatchId} | {error, Reason}
watch(TopicString, CallBack) ->
    watch(TopicString, CallBack, []).

%% @doc Same as `watch/2', except that `UserInfo' will be passed as a third argument
%% to the callback if the callback takes three arguments.
%% @spec watch( TopicString :: string(), CallBack, UserInfo ) -> {ok, WatchId} | {error, Reason}
watch(TopicString, CallBack, UserInfo) ->
    watch(TopicString, CallBack, UserInfo, ?TRILLION).

%% @doc Same as `watch/3', except that the watch expires after `TTL' seconds.
%% @spec watch( TopicString :: string(), CallBack, UserInfo, TTL) -> {ok, WatchId} | {error, Reason}
watch(TopicString, CallBack, UserInfo, TTL) ->
    gen_server:call({global, ?MODULE}, {watch, TopicString, CallBack, UserInfo, TTL}).

%% @doc Create or replace a watch with `WatchId'.
%% @spec set_watch( WatchId, TopicString::string(), CallBack ) -> ok | {error, Reason}
set_watch(WatchId, TopicString, CallBack) ->
    set_watch(WatchId, TopicString, CallBack, []).

%% @doc Same as `set_watch/3', except that `UserInfo' is passed to the callback.
%% @spec set_watch( WatchId, TopicString::string(), CallBack, UserInfo ) -> ok | {error, Reason}
set_watch(WatchId, TopicString, CallBack, UserInfo) ->
    set_watch(WatchId, TopicString, CallBack, UserInfo, ?TRILLION).

%% @doc Same as `set_watch/4', except that the watch expires after `TTL' seconds.
%% @spec set_watch( WatchId, TopicString::string(), CallBack, UserInfo, TTL ) -> ok | {error, Reason}
set_watch(WatchId, TopicString, CallBack, UserInfo, TTL) ->
    gen_server:call({global, ?MODULE}, {set_watch, WatchId, TopicString, CallBack, UserInfo, TTL}).

%% @doc Cancel an existing watch identified by `WatchId'.
%% @spec cancel_watch( WatchId ) -> ok | {error, Reason}
cancel_watch(WatchId) ->
    gen_server:call({global, ?MODULE}, {cancel_watch, WatchId}).

%% @doc Extend an existing watch by the time-to-live specified at creation time.
%% @spec extend_watch( WatchId ) -> ok | {error, Reason}
extend_watch(WatchId) ->
    gen_server:call({global, ?MODULE}, {extend_watch, WatchId}).

deleted(Id, Attrs) ->
    gen_server:call({global, ?MODULE}, {deleted, Id, Attrs}).

updated(Id, OldAttrs, NewAttrs) ->
    gen_server:call({global, ?MODULE}, {updated, Id, OldAttrs, NewAttrs}).

created(Id, NewAttrs) ->
    gen_server:call({global, ?MODULE}, {created, Id, NewAttrs}).

reset() ->
    gen_server:call({global, ?MODULE}, reset).

dump() ->
    gen_server:call({global, ?MODULE}, dump).
