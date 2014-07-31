-module(boss_env).

-export([boss_env/0, setup_boss_env/0, get_env/2, get_env/3]).
-export([master_node/0, is_master_node/0, is_developing_app/1]).
-export([router_adapter/0, cache_adapter/0, session_adapter/0, mq_adapter/0]).

-spec boss_env() -> any().
boss_env() ->
    case get(boss_environment) of
        undefined -> setup_boss_env();
        Val -> Val
    end.

-spec setup_boss_env() -> types:execution_mode().
setup_boss_env() ->	
    case boss_load:module_is_loaded(reloader) of
        true  -> put(boss_environment, development), development;
        false -> put(boss_environment, production), production
    end.			

-spec get_env(atom(),atom(),any()) -> any().
get_env(App, Key, Default) when is_atom(App), is_atom(Key) ->
    case application:get_env(App, Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

-spec get_env(atom(), any()) -> any().
get_env(Key, Default) when is_atom(Key) ->
    get_env(boss, Key, Default).

-spec master_node() -> any().
master_node() ->
    case boss_env:get_env(master_node, erlang:node()) of
        MasterNode when is_list(MasterNode) ->
            list_to_atom(MasterNode);
        MasterNode -> MasterNode
    end.

-spec is_master_node() -> boolean().
is_master_node() ->
    master_node() =:= erlang:node().

-spec is_developing_app(types:application()) -> boolean().
is_developing_app(AppName) ->
    BossEnv		= boss_env:boss_env(),
    DevelopingApp	= boss_env:get_env(developing_app, undefined),
    AppName		=:= DevelopingApp andalso BossEnv =:= development.

-spec router_adapter() -> atom().
router_adapter() ->
	get_env(router_adapter, boss_router).

-spec cache_adapter() -> atom().
cache_adapter() ->
	get_env(cache_adapter, memcached_bin).

-spec session_adapter() -> atom().
session_adapter() ->
	get_env(session_adapter, mock).

-spec mq_adapter() -> atom().
mq_adapter() ->
	get_env(mq_adapter, tinymq).
