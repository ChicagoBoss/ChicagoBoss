-module(boss_template_adapter_erlydtl).
-compile(export_all).

-spec file_extensions() -> [[100 | 104 | 106 | 108 | 109 | 115 | 116 | 120,...],...].
-spec translatable_strings(atom() | tuple()) -> any().
-spec source(atom() | tuple()) -> any().
-spec dependencies(atom() | tuple()) -> any().
-spec render(atom() | tuple(),_,_) -> {ok, iolist()}|{error, _}.
-spec compile_file(atom() | binary() | [atom() | [any()] | char()],_,[any()]) -> any().
-spec compile(atom() | binary() | [atom() | [any()] | char()],_,_,_,_,_,_,_,[any()],[any()],[any()],_,_) -> any().
file_extensions() -> ["dtl", "html", "txt", "js"].

translatable_strings(Module) ->
    Module:translatable_strings().

source(Module) ->
    Module:source().

dependencies(Module) ->
    Module:dependencies().


render(Module, Variables, RenderOptions) ->
    Module:render(Variables, RenderOptions).

compile_file(ViewPath, Module, Options) ->
    HelperDirModule	= proplists:get_value(helper_module,	 Options),
    TranslatorPid	= proplists:get_value(translator_pid,	 Options),
    OutDir		= proplists:get_value(out_dir,		 Options),
    CompilerOptions	= proplists:get_value(compiler_options,	 Options, []),
    Locales		= proplists:get_value(locales,		 Options, []),
    DocRoot		= proplists:get_value(doc_root,		 Options, "."),
    TagHelpers		= proplists:get_value(tag_helpers,	 Options, []),
    FilterHelpers	= proplists:get_value(filter_helpers,	 Options, []),
    ExtraTagHelpers	= boss_env:get_env(template_tag_modules,          []),
    ExtraFilterHelpers	= boss_env:get_env(template_filter_modules,       []),
    AutoEscape		= boss_env:get_env(template_auto_escape,          true),
    Res                 = compile(ViewPath, Module, HelperDirModule, TranslatorPid, OutDir,
				  CompilerOptions, Locales, DocRoot, TagHelpers, FilterHelpers,
				  ExtraTagHelpers, ExtraFilterHelpers, AutoEscape),
    case Res of
        {ok, Module} -> {ok, Module};
        {error, Errors} -> {ok, Errors}
    end.


compile(ViewPath, Module, HelperDirModule, TranslatorPid, OutDir,
	CompilerOptions, Locales, DocRoot, TagHelpers, FilterHelpers,
        ExtraTagHelpers, ExtraFilterHelpers, AutoEscape) ->
    CompileParams = [{doc_root, DocRoot},
	             {custom_tags_modules, TagHelpers ++ ExtraTagHelpers ++ [boss_erlydtl_tags, HelperDirModule]},
		     {custom_filters_modules, FilterHelpers ++ ExtraFilterHelpers},
		     {compiler_options, CompilerOptions},
		     {out_dir, OutDir},
		     return,
		     {blocktrans_fun, make_blocktrans_fun(TranslatorPid)},
		     {blocktrans_locales, Locales},
		     {auto_escape, AutoEscape}],
    Res = erlydtl:compile_file(ViewPath,
                    Module,
                    CompileParams),
    case Res of
        {ok, Module} -> {ok, Module};
        {ok, Module, _Warnings} -> {ok, Module};
        {error, Errors, _Warnings} -> {error, Errors}
    end.

make_blocktrans_fun(TranslatorPid) ->
    fun(BlockString, Locale) ->
	case boss_translator:lookup(TranslatorPid, BlockString, Locale) of
	    undefined -> default;
	    Body -> list_to_binary(Body)
	end
    end.
