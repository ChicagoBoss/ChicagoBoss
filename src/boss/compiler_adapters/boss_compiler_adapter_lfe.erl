-module(boss_compiler_adapter_lfe).
-compile(export_all).

file_extensions() -> ["lfe"].

controller_module(AppName, Controller) -> lists:concat([AppName, "_", Controller, "_controller"]).

module_name_for_file(_AppName, File) -> filename:basename(File, ".lfe").

compile_controller(File, Options) ->
    lfe_comp:file(File, [verbose, return | proplists:get_value(compiler_options, Options, [])]).

compile(File, Options) ->
    lfe_comp:file(File, [verbose, return | proplists:get_value(compiler_options, Options, [])]).
