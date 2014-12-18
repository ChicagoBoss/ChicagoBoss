%%
%% $Id: $
%%
%% Module:  boss_model -- description
%% Created: 01-MAY-2012 16:32
%% Author:  tmr
%%

-module(boss_model_manager_boss_db).
-behaviour(boss_model_manager_adapter).
-export([
  start/0,
  stop/0,
  compile/2,
  edoc_module/2,
  is_model_instance/2,
  dummy_instance/1,
  to_json/1 %, from_json/1
]).

start() ->
    DBOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [db_port, db_host, db_username, db_password, db_database, 
            db_replication_set, db_read_mode, db_write_mode, 
            db_write_host, db_write_host_port, db_read_capacity, 
            db_write_capacity, db_model_read_capacity, db_model_write_capacity]),

    DBAdapter = boss_env:get_env(db_adapter, mock),
    DBShards = boss_env:get_env(db_shards, []),
    CacheEnable = boss_env:get_env(cache_enable, false),
    IsMasterNode = boss_env:is_master_node(),
    CachePrefix = boss_env:get_env(cache_prefix, db),
    DBCacheEnable = boss_env:get_env(db_cache_enable, false) andalso CacheEnable,
    DBOptions1 = [{adapter, DBAdapter}, {cache_enable, DBCacheEnable}, {cache_prefix, CachePrefix},
        {shards, DBShards}, {is_master_node, IsMasterNode}|DBOptions],

    boss_db:start(DBOptions1).

stop() ->
    boss_db:stop().

compile(ModulePath, CompilerOptions) ->
  boss_record_compiler:compile(ModulePath, CompilerOptions).

edoc_module(ModulePath, Options) ->
  boss_record_compiler:edoc_module(ModulePath, Options).

is_model_instance(Object, AvailableModels) ->
  boss_record_lib:is_boss_record(Object, AvailableModels).

dummy_instance(Model) ->
  boss_record_lib:dummy_record(Model).

to_json(Object) ->
  Data = lists:map (fun
    ({Attr, Val}) when is_list (Val) ->
       {Attr, list_to_binary (Val)};
    ({Attr, {_,_,_} = Val}) ->
       {Attr, iso8601:format(Val)};
    ({Attr, {{_, _, _}, {_, _, _}} = Val}) ->
       {Attr, iso8601:format(Val)};
    (Other) ->
       Other
  end, Object:attributes()),
  {struct, Data}.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
