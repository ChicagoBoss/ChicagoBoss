-module(boss_template_adapter_erlydtl).
-compile(export_all).

file_extensions() -> ["dtl", "html", "txt"].

translatable_strings(Module) ->
    Module:translatable_strings().

source(Module) ->
    Module:source().

dependencies(Module) ->
    Module:dependencies().

render(Module, Variables, RenderOptions) ->
    Module:render(Variables, RenderOptions).

compile_file(ViewPath, Module, Options) ->
    HelperDirModule = proplists:get_value(helper_module, Options),
    TranslatorPid = proplists:get_value(translator_pid, Options),
    OutDir = proplists:get_value(out_dir, Options),
    CompilerOptions = proplists:get_value(compiler_options, Options, []),
    Locales = proplists:get_value(locales, Options, []),
    DocRoot = proplists:get_value(doc_root, Options, "."),
    TagHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_tag_helper_list()),
    FilterHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_filter_helper_list()),
    ExtraTagHelpers = boss_env:get_env(template_tag_modules, []),
    ExtraFilterHelpers = boss_env:get_env(template_filter_modules, []),
    Res = erlydtl_compiler:compile(ViewPath, Module,
        [{doc_root, DocRoot}, 
            {custom_tags_modules, TagHelpers ++ ExtraTagHelpers ++ [boss_erlydtl_tags, HelperDirModule]},
            {custom_filters_modules, FilterHelpers ++ ExtraFilterHelpers},
            {compiler_options, CompilerOptions}, {out_dir, OutDir}, {blocktrans_fun,
                fun(BlockString, Locale) ->
                        case boss_translator:lookup(TranslatorPid, BlockString, Locale) of
                            undefined -> default;
                            Body -> list_to_binary(Body)
                        end
                end}, {blocktrans_locales, Locales}]),
    case Res of
        ok -> {ok, Module};
        Err -> Err
    end.
