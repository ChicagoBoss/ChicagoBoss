-module(boss_template_adapter_eex).
-compile(export_all).

-define(TEMPLATE_VARIABLE_KEY, boss_template_adapter_eex_vars).

file_extensions() -> ["eex"].

translatable_strings(_Module) -> [].

source(_Module) -> "".

dependencies(_Module) -> [].

% stupid hack for now
assigns() ->
    get(?TEMPLATE_VARIABLE_KEY).

render(Module, Variables, _Options) ->
    put(?TEMPLATE_VARIABLE_KEY, Variables),
    {ok, Module:render()}.

compile_file(ViewPath, Module, Options) ->
    OutDir = proplists:get_value(out_dir, Options),
    CompilerOptions = proplists:get_value(compiler_options, Options, []),
    EExAst = 'Elixir-EEx':compile_file(ViewPath),
    Scope = elixir:scope_for_eval([{delegate_locals_to, ?MODULE}]),
    {ErlAst, _Scope1} = elixir_translator:translate_each(EExAst, Scope),

    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([], none, [ErlAst])]),

    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),

    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), 
                        erl_syntax:integer(0))])]),

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
