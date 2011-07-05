-module(boss_env).

-export([boss_env/0, setup_boss_env/0, get_env/2, is_master_node/0]).

boss_env() ->
    case get(boss_environment) of
        undefined -> setup_boss_env();
        Val -> Val
    end.

setup_boss_env() ->	
    case boss_load:module_is_loaded(reloader) of
        true -> put(boss_environment, development), development;
        false -> put(boss_environment, production), production
    end.			

get_env(Key, Default) when is_atom(Key) ->
    case application:get_env(Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

is_master_node() ->
    boss_env:get_env(master_node, erlang:node()) =:= erlang:node().
