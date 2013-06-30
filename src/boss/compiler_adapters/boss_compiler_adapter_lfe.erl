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
    {OutFile, CompilerOptions} = lfe_compiler_options(File, Options),
    case lfe_comp:file(File, CompilerOptions) of
        {ok, Module, _Warnings} ->
            io:format("Loading ~p~n", [OutFile]),
            {module, Module} = code:load_abs(OutFile),
            {ok, Module};
        Other ->
            Other
    end.

lfe_compiler_options(File, Options) ->
    CompilerOptions = [verbose, return, proplists:get_value(compiler_options, Options, [])],
    WriteDir = case proplists:get_value(out_dir, Options) of
        undefined -> "/tmp";
        Dir -> Dir
    end,
    WriteFile = filename:join([WriteDir, filename:basename(File, ".lfe")]),
    {WriteFile, [{outdir, WriteDir}|CompilerOptions]}.
