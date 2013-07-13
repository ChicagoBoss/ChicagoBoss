-module(boss_template_adapter_eex).
-compile(export_all).

file_extensions() -> ["eex"].

translatable_strings(_Module) -> [].

source(_Module) -> "".

dependencies(_Module) -> [].

render(Module, Variables, _Options) ->
    {ok, Module:render(Variables)}.

compile_file(ViewPath, Module, Options) ->
    OutDir = proplists:get_value(out_dir, Options),
    CompilerOptions = proplists:get_value(compiler_options, Options, []),
    EExAst = 'Elixir.EEx':compile_file(ViewPath),
    {ErlAst, _} = elixir:translate_forms([EExAst], [{ assigns, [] }], [{delegate_locals_to, ?MODULE}]),

    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables")], none, [
                    {match, 0, {var, 0, assigns}, {var, 0, 'Variables'}} | ErlAst])]),

    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),

    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), 
                        erl_syntax:integer(1))])]),

    Forms = [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, Render0FunctionAst]],

    case compile_forms_and_reload(ViewPath, Forms, CompilerOptions) of
        {ok, Module, Bin, _Warnings} ->
            case OutDir of
                undefined -> {ok, Module};
                _ ->
                    BeamFile = filename:join([OutDir, atom_to_list(Module) ++ ".beam"]),
                    case file:write_file(BeamFile, Bin) of
                        ok -> {ok, Module};
                        {error, Reason} ->
                            {error, lists:flatten(
                                    io_lib:format("Beam generation of '~s' failed: ~p",
                                        [BeamFile, file:format_error(Reason)]))}
                    end
            end;
        Err ->
            Err
    end.

compile_forms_and_reload(File, Forms, CompilerOptions) ->
    case compile:forms(Forms, CompilerOptions) of
        {ok, Module1, Bin} ->
            load_code(Module1, Bin, []);
        {ok, Module1, Bin, Warnings} ->
            load_code(Module1, Bin, Warnings);
        error ->
            {error, lists:concat(["compilation failed: ", File])};
        OtherError ->
            OtherError
    end.

load_code(Module, Bin, Warnings) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
        {module, _} -> {ok, Module, Bin, Warnings};
        _ -> {error, lists:concat(["code reload failed: ", Module])}
    end.
