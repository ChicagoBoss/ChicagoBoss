-module(boss_lang_filter).
-export([default_config/0, before_filter/2, after_filter/3]).

default_config() -> auto.

before_filter(auto, RequestContext) ->
    {ok, proplists:delete(language, RequestContext)};
before_filter(Language, RequestContext) ->
    {ok, proplists:delete(language, RequestContext) ++ [{language, Language}]}.

after_filter({ok, Payload, Headers}, auto, _) -> 
    {ok, Payload, Headers};
after_filter({ok, Payload, Headers}, Language, _) -> 
    {ok, Payload, boss_web_controller:merge_headers(Headers, [{"Content-Language", Language}])};
after_filter(Other, _, _) -> Other.
