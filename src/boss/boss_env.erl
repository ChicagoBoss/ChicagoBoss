-module(boss_env).

-export([boss_env/0, setup_boss_env/0, get_env/2, get_env/3]).
-export([master_node/0, is_master_node/0, is_developing_app/1]).

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

get_env(App, Key, Default) when is_atom(App), is_atom(Key) ->
    case application:get_env(App, Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

get_env(Key, Default) when is_atom(Key) ->
    get_env(boss, Key, Default).

master_node() ->
    case boss_env:get_env(master_node, erlang:node()) of
        MasterNode when is_list(MasterNode) ->
            list_to_atom(MasterNode);
        MasterNode -> MasterNode
    end.

is_master_node() ->
    master_node() =:= erlang:node().

is_developing_app(AppName) ->
    BossEnv = boss_env:boss_env(),
    DevelopingApp = boss_env:get_env(developing_app, undefined),
    AppName =:= DevelopingApp andalso BossEnv =:= development.
