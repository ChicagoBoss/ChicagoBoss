%%
%% $Id: $
%%
%% Module:  boss_model -- description
%% Created: 01-MAY-2012 16:32
%% Author:  tmr
%%

-module (boss_model_manager).
-behaviour (boss_model_manager_adapter).
-export ([
  compile/1, compile/2,
  edoc_module/1, edoc_module/2,

  is_model_instance/2,
  dummy_instance/1,

  to_json/1 %, from_json/1
]).

get_adapter () ->
  list_to_atom (lists:concat (["boss_model_manager_",
    boss_env:get_env (model_manager, boss_db)])).

compile (ModulePath) -> compile (ModulePath, []).
compile (ModulePath, CompilerOptions) ->
  (get_adapter ()):compile (ModulePath, CompilerOptions).

edoc_module (ModulePath) -> edoc_module (ModulePath, []).
edoc_module (ModulePath, Options) ->
  (get_adapter ()):edoc_module (ModulePath, Options).

is_model_instance (Object, AvailableModels) ->
  (get_adapter ()):is_model_instance (Object, AvailableModels).

dummy_instance (Model) ->
  (get_adapter ()):dummy_instance (Model).

to_json (Object) ->
  (get_adapter ()):to_json (Object).

%from_json (Data) ->
%  (get_adapter ()):from_json (Data).

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
