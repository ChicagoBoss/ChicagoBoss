%%
%% $Id: $
%%
%% Module:  boss_model -- description
%% Created: 01-MAY-2012 16:32
%% Author:  tmr
%%

-module (boss_model_manager_boss_db).
-behaviour (boss_model_manager_adapter).
-export ([
  compile/1, compile/2,
  edoc_module/1, edoc_module/2,

  is_model_instance/2,
  dummy_instance/1,

  to_json/1 %, from_json/1
]).

compile (ModulePath) ->
  boss_record_compiler:compile (ModulePath).
compile (ModulePath, CompilerOptions) ->
  boss_record_compiler:compile (ModulePath, CompilerOptions).

edoc_module (ModulePath) ->
  boss_record_compiler:edoc_module (ModulePath).
edoc_module (ModulePath, Options) ->
  boss_record_compiler:edoc_module (ModulePath, Options).

is_model_instance (Object, AvailableModels) ->
  boss_record_lib:is_boss_record (Object, AvailableModels).

dummy_instance (Model) ->
  boss_record_lib:dummy_record (Model).

to_json (Object) ->
  Data = lists:map (fun
    ({Attr, Val}) when is_list (Val) ->
       {Attr, list_to_binary (Val)};
    ({Attr, {_,_,_} = Val}) ->
       {Attr, erlydtl_filters:date (calendar:now_to_datetime (Val), "F d, Y H:i:s")};
    ({Attr, {{_, _, _}, {_, _, _}} = Val}) ->
       {Attr, list_to_binary (erlydtl_filters:date (Val, "F d, Y H:i:s"))};
    (Other) ->
       Other
  end, Object:attributes ()),
  {struct, Data}.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
