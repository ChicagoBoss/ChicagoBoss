-module(boss_compiler_adapter_lfe).
-compile(export_all).

file_extensions() -> ["lfe"].

controller_module(AppName, Controller) -> lists:concat([AppName, "_", Controller, "_controller"]).

module_name_for_file(_AppName, File) -> filename:basename(File, ".lfe").

compile_controller(File, Options) ->
    do_compile(File, Options). 

compile(File, Options) ->
    do_compile(File, Options).

do_compile(File, Options) ->
    CompilerOptions = lfe_compiler_options(Options),
    case lfe_comp:file(File, CompilerOptions) of
        {ok, Module, Binary, _Warnings} ->
            {module, Module} = code:load_binary(Module, File, Binary),
            ok = case proplists:get_value(out_dir, Options) of
                undefined -> ok;
                OutDir ->
                    OutFile = filename:join([OutDir, filename:basename(File, ".lfe") ++ ".beam"]),
                    file:write_file(OutFile, Binary)
            end,
            {ok, Module};
        Other ->
            Other
    end.

lfe_compiler_options(Options) ->
    [verbose, return, binary] ++ proplists:get_value(compiler_options, Options, []).
