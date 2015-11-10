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

-module(boss_lang_filter).
-export([config_default_value/0, config_key/0, before_filter/2, after_filter/3]).

config_default_value() -> auto.
config_key() -> lang.

before_filter(auto, RequestContext) ->
    {ok, proplists:delete(language, RequestContext)};
before_filter(Language, RequestContext) ->
    {ok, proplists:delete(language, RequestContext) ++ [{language, Language}]}.

after_filter({ok, Payload, Headers}, auto, _) ->
    {ok, Payload, Headers};
after_filter({ok, Payload, Headers}, Language, _) ->
    {ok, Payload, boss_web_controller:merge_headers(Headers, [{"Content-Language", Language}])};
after_filter(Other, _, _) -> Other.
