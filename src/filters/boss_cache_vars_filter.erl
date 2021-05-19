%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc
%%-------------------------------------------------------------------

-module(boss_cache_vars_filter).
-export([config_key/0, config_default_value/0]).
-export([before_filter/2, middle_filter/3]).

-define(VARIABLES_CACHE_PREFIX, "boss_web_controller_variables").
-define(VARIABLES_CACHE_DEFAULT_TTL, 60).

config_key() -> cache.
config_default_value() -> none.

before_filter({vars, _}, RequestContext) ->
    EffectiveRequestMethod = case proplists:get_value(method, RequestContext) of
        'HEAD' -> 'GET';
        Method -> Method
    end,

    case (boss_env:get_env(cache_enable, false) andalso
            EffectiveRequestMethod =:= 'GET') of
        true ->
            Language = proplists:get_value(language, RequestContext, auto),
            ControllerModule = proplists:get_value(controller_module, RequestContext),
            Action = proplists:get_value(action, RequestContext),
            Tokens = proplists:get_value(tokens, RequestContext, []),
            Req = proplists:get_value(request, RequestContext, []),
            Query = Req:query_params(),

            CacheKey = {ControllerModule, Action, Tokens, Language, Query},

            case boss_cache:get(?VARIABLES_CACHE_PREFIX, CacheKey) of
                undefined ->
                    _ = lager:debug("cache: vars not in cache"),
                    {ok, RequestContext ++ [{cache_vars, true}, {cache_key, CacheKey}]};
                CachedActionResult ->
                    _ = lager:debug("cache: hit!"),
                    CachedActionResult
            end;
        false ->
            {ok, RequestContext}
    end;
before_filter(_, RequestContext) ->
    {ok, RequestContext}.

middle_filter({render, _, _} = ActionResult, {vars, CacheOptions}, RequestContext) ->
    case proplists:get_value(cache_vars, RequestContext, false) of
        true ->
            CacheKey = proplists:get_value(cache_key, RequestContext),
            CacheTTL = proplists:get_value(seconds, CacheOptions, ?VARIABLES_CACHE_DEFAULT_TTL),
            case proplists:get_value(watch, CacheOptions) of
                undefined -> ok;
                CacheWatchString ->
                    boss_news:set_watch({?VARIABLES_CACHE_PREFIX, CacheKey}, CacheWatchString,
                        fun ?MODULE:handle_news_for_cache/3, {?VARIABLES_CACHE_PREFIX, CacheKey},
                        CacheTTL)
            end,
            _ = lager:debug("cache: saving vars"),
            boss_cache:set(?VARIABLES_CACHE_PREFIX, CacheKey, ActionResult, CacheTTL);
        false ->
            ok
    end,
    ActionResult;
middle_filter(Other, _, _) ->
    Other.
