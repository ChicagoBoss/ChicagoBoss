-module(boss_cache_page_filter).
-export([config_key/0, config_default_value/0]).
-export([before_filter/2, middle_filter/3, after_filter/3]).

-define(PAGE_CACHE_PREFIX, "boss_web_controller_page").
-define(PAGE_CACHE_DEFAULT_TTL, 60).

config_key() -> cache.
config_default_value() -> none.

before_filter({page, _}, RequestContext) ->
    EffectiveRequestMethod = case proplists:get_value(method, RequestContext) of
        'HEAD' -> 'GET';
        Method -> Method
    end,
    case (boss_env:get_env(cache_enable, false) andalso EffectiveRequestMethod =:= 'GET') of
        true ->
            Language = proplists:get_value(language, RequestContext, auto),
            ControllerModule = proplists:get_value(controller_module, RequestContext),
            Action = proplists:get_value(action, RequestContext),
            Tokens = proplists:get_value(tokens, RequestContext, []),

            CacheKey = {ControllerModule, Action, Tokens, Language},

            case boss_cache:get(?PAGE_CACHE_PREFIX, CacheKey) of
                undefined ->
                    {ok, RequestContext ++ [{cache_page, true}, {cache_key, CacheKey}]};
                CachedRenderedResult ->
                    {cached_page, CachedRenderedResult}
            end;
        false ->
            {ok, RequestContext}
    end;
before_filter(_, RequestContext) ->
    {ok, RequestContext}.

middle_filter({cached_page, CachedRenderedResult}, _CacheInfo, _RequestContext) ->
    CachedRenderedResult;
middle_filter(Other, _CacheInfo, _RequestContext) ->
    Other.

after_filter({ok, _, _} = Rendered, {page, CacheOptions}, RequestContext) ->
    case proplists:get_value(cache_page, RequestContext, false) of
        true ->
            CacheKey = proplists:get_value(cache_key, RequestContext),
            CacheTTL = proplists:get_value(seconds, CacheOptions, ?PAGE_CACHE_DEFAULT_TTL),
            case proplists:get_value(watch, CacheOptions) of
                undefined -> ok;
                CacheWatchString ->
                    boss_news:set_watch({?PAGE_CACHE_PREFIX, CacheKey}, CacheWatchString,
                        fun boss_web_controller:handle_news_for_cache/3, {?PAGE_CACHE_PREFIX, CacheKey}, CacheTTL)
            end,
            boss_cache:set(?PAGE_CACHE_PREFIX, CacheKey, Rendered, CacheTTL);
        false ->
            ok
    end,
    Rendered;
after_filter(Rendered, _, _) ->
    Rendered.
