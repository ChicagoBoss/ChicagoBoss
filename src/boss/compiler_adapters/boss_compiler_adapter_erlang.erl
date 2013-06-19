-module(boss_compiler_adapter_erlang).
-compile(export_all).

file_extensions() -> ["erl"].

controller_module(AppName, Controller) -> lists:concat([AppName, "_", Controller, "_controller"]).

module_name_for_file(_AppName, File) -> filename:basename(File, ".erl").

compile_controller(File, Options) ->
    boss_controller_compiler:compile(File, Options).

compile(File, Options) ->
    boss_compiler:compile(File, Options).
