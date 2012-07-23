-module(boss_load).
-export([
        incoming_mail_controller_module/1,
        load_all_modules/2,
        load_all_modules/3,
        load_all_modules_and_emit_app_file/2,
        load_libraries/0,
        load_services_websockets/0,
        load_mail_controllers/0,
        load_models/0,
        load_view_if_dev/3,
        load_view_lib_modules/0,
        load_web_controllers/0,
        module_is_loaded/1,
        reload_all/0
    ]).

-define(CUSTOM_TAGS_DIR_MODULE, '_view_lib_tags').

load_all_modules(Application, TranslatorPid) ->
    load_all_modules(Application, TranslatorPid, undefined).

load_all_modules(Application, TranslatorPid, OutDir) ->
    {ok, TestModules} = load_dirs(boss_files:test_path(), OutDir, fun compile/2),
    {ok, LibModules} = load_libraries(OutDir),
    {ok, WebSocketModules} = load_services_websockets(OutDir),
    {ok, MailModules} = load_mail_controllers(OutDir),
    {ok, ControllerModules} = load_web_controllers(OutDir),
    {ok, ModelModules} = load_models(OutDir),
    {ok, ViewHelperModules} = load_view_lib_modules(OutDir),
    {ok, ViewLibModules} = load_view_lib(Application, OutDir, TranslatorPid),
    {ok, ViewModules} = load_views(Application, OutDir, TranslatorPid),
    AllModules = [{test_modules, TestModules}, 
		  {lib_modules, LibModules}, {websocket_modules, WebSocketModules},
		  {mail_modules, MailModules}, {controller_modules, ControllerModules},
		  {model_modules, ModelModules}, {view_lib_tags_modules, ViewLibModules},
		  {view_lib_helper_modules, ViewHelperModules}, {view_modules, ViewModules}],
    {ok, AllModules}.

load_all_modules_and_emit_app_file(AppName, OutDir) ->
    TranslatorPid = boss_translator:start([{application, AppName}]),
    {ok, ModulePropList} = load_all_modules(AppName, TranslatorPid, OutDir),
    AllModules = lists:foldr(fun({_, Mods}, Acc) -> Mods ++ Acc end, [], ModulePropList),
	DotAppSrc = boss_files:dot_app_src(AppName),
    {ok, [{application, AppName, AppData}]} = file:consult(DotAppSrc),
    AppData1 = lists:keyreplace(modules, 1, AppData, {modules, AllModules}),
    Vsn = proplists:get_value(vsn, AppData1, []),
    ComputedVsn = case vcs_vsn_cmd(Vsn) of
        {unknown, Val} -> Val;
        Cmd ->
            VsnString = os:cmd(Cmd),
            string:strip(VsnString, right, $\n)
    end,
    AppData2 = lists:keyreplace(vsn, 1, AppData1, {vsn, ComputedVsn}),
    DefaultEnv = proplists:get_value(env, AppData2, []),
    AppData3 = lists:keyreplace(env, 1, AppData2, {env, ModulePropList ++ DefaultEnv}),

    IOList = io_lib:format("~p.~n", [{application, AppName, AppData3}]),
    AppFile = filename:join([OutDir, lists:concat([AppName, ".app"])]),
    file:write_file(AppFile, IOList).

reload_all() ->
    Modules = [M || {M, F} <- code:all_loaded(), is_list(F), not code:is_sticky(M)],
    [begin code:purge(M), code:load_file(M) end || M <- Modules].

load_libraries() ->
    load_libraries(undefined).
load_libraries(OutDir) ->
    load_dirs(boss_files:lib_path(), OutDir, fun compile/2).

load_services_websockets() ->
    OutDir = boss_files:ebin_dir(),
    error_logger:info_msg("websockets_service: outdir:~p~n", [OutDir]),
    load_services_websockets(OutDir).
load_services_websockets(OutDir) ->
    load_dirs(boss_files:websocket_dir_path(), OutDir, fun compile/2).

load_mail_controllers() ->
    load_mail_controllers(undefined).
load_mail_controllers(OutDir) ->
    load_dirs(boss_files:mail_controller_path(), OutDir, fun compile/2).

load_web_controllers() ->
    load_web_controllers(undefined).
load_web_controllers(OutDir) ->
    load_dirs(boss_files:web_controller_path(), OutDir, fun compile_controller/2).

load_view_lib_modules() ->
    load_view_lib_modules(undefined).
load_view_lib_modules(OutDir) ->
    load_dirs(boss_files:view_helpers_path(), OutDir, fun compile/2).

load_models() ->
    load_models(undefined).
load_models(OutDir) ->
    load_dirs(boss_files:model_path(), OutDir, fun compile_model/2).

load_dirs(Dirs, OutDir, Compiler) ->
    load_dirs1(Dirs, OutDir, Compiler, [], []).

load_dirs1([], _, _, ModuleAcc, []) ->
    {ok, ModuleAcc};
load_dirs1([], _, _, _, ErrorAcc) ->
    {error, ErrorAcc};
load_dirs1([Dir|Rest], OutDir, Compiler, ModuleAcc, ErrorAcc) ->
    case load_dir(Dir, OutDir, Compiler) of
        {ok, ModuleList} ->
            load_dirs1(Rest, OutDir, Compiler, ModuleList ++ ModuleAcc, ErrorAcc);
        {error, ErrorList} ->
            load_dirs1(Rest, OutDir, Compiler, ModuleAcc, ErrorList ++ ErrorAcc)
    end.

load_dir(Dir, OutDir, Compiler) when is_function(Compiler) ->
    Files = case file:list_dir(Dir) of
        {ok, FileList} ->
            FileList;
        _ ->
            []
    end,
    {ModuleList, ErrorList} = lists:foldl(fun
            ("."++_, Acc) ->
                Acc;
            (File, {Modules, Errors}) ->
                Filename = filename:join([Dir, File]),
                case filelib:is_dir(Filename) of
                    true ->
                        case load_dir(Filename, OutDir, Compiler) of
                            {ok, NewMods} ->
                                {NewMods ++ Modules, Errors};
                            {error, NewErrs} ->
                                {Modules, NewErrs ++ Errors}
                        end;
                    false ->
                        case maybe_compile(Dir, File, OutDir, Compiler) of
                            ok ->
                                {Modules, Errors};
                            {ok, Module} ->
                                {[Module|Modules], Errors};
                            {error, Error} ->
                                {Modules, [Error | Errors]};
                            {error, NewErrors, _NewWarnings} when is_list(NewErrors) ->
                                {Modules, NewErrors ++ Errors}
                        end
                end
        end, {[], []}, Files),
    case length(ErrorList) of
        0 ->
            {ok, ModuleList};
        _ ->
            {error, ErrorList}
    end.

maybe_compile(Dir, File, OutDir, Compiler) ->
    case lists:suffix(".erl", File) of
        true ->
            ModuleName = filename:basename(File, ".erl"),
            Module = list_to_atom(ModuleName),
            AbsPath = filename:absname(filename:join([Dir, File])),
            case OutDir of
                undefined ->
                    case module_older_than(Module, [AbsPath]) of
                        true ->
                            Compiler(AbsPath, OutDir);
                        _ ->
                            {ok, Module}
                    end;
                _ ->
                    Compiler(AbsPath, OutDir)
            end;
        _ ->
            ok
    end.

view_doc_root(ViewPath) ->
    lists:foldl(fun
            (LibPath, Best) when length(LibPath) > length(Best) ->
                case lists:prefix(LibPath, ViewPath) of
                    true ->
                        LibPath;
                    false ->
                        Best
                end;
            (_, Best) ->
                Best
        end, "", 
        [boss_files:web_view_path(), boss_files:mail_view_path()]).

compile_view_dir_erlydtl(LibPath, Module, OutDir, TranslatorPid) ->
    TagHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_tag_helper_list()),
    FilterHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_filter_helper_list()),
    ExtraTagHelpers = boss_env:get_env(template_tag_modules, []),
    ExtraFilterHelpers = boss_env:get_env(template_filter_modules, []),
    Res = erlydtl_compiler:compile_dir(LibPath, Module,
        [{doc_root, view_doc_root(LibPath)}, {compiler_options, []}, {out_dir, OutDir}, 
            {custom_tags_modules, TagHelpers ++ ExtraTagHelpers ++ [boss_erlydtl_tags]},
            {custom_filters_modules, FilterHelpers ++ ExtraFilterHelpers},
            {blocktrans_fun, fun(BlockString, Locale) ->
                    case boss_translator:lookup(TranslatorPid, BlockString, Locale) of
                        undefined -> default;
                        Body -> list_to_binary(Body)
                    end
            end}]),
    case Res of
        ok -> {ok, Module};
        Err -> Err
    end.

compile_view_erlydtl(Application, ViewPath, OutDir, TranslatorPid) ->
    HelperDirModule = view_custom_tags_dir_module(Application),
    TagHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_tag_helper_list()),
    FilterHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_filter_helper_list()),
    ExtraTagHelpers = boss_env:get_env(template_tag_modules, []),
    ExtraFilterHelpers = boss_env:get_env(template_filter_modules, []),
    Module = view_module(Application, ViewPath),
    Res = erlydtl_compiler:compile(ViewPath, Module,
        [{doc_root, view_doc_root(ViewPath)}, 
            {custom_tags_modules, TagHelpers ++ ExtraTagHelpers ++ [boss_erlydtl_tags, HelperDirModule]},
            {custom_filters_modules, FilterHelpers ++ ExtraFilterHelpers},
            {compiler_options, []}, {out_dir, OutDir}, {blocktrans_fun,
                fun(BlockString, Locale) ->
                        case boss_translator:lookup(TranslatorPid, BlockString, Locale) of
                            undefined -> default;
                            Body -> list_to_binary(Body)
                        end
                end}, {blocktrans_locales, boss_files:language_list(Application)}]),
    case Res of
        ok -> {ok, Module};
        Err -> Err
    end.

compile_model(ModulePath, OutDir) ->
    boss_model_manager:compile(ModulePath, [{out_dir, OutDir}, {include_dirs, [boss_files:include_dir()]}]).

compile_controller(ModulePath, OutDir) ->
    boss_controller_compiler:compile(ModulePath, [{out_dir, OutDir}, {include_dirs, [boss_files:include_dir()]}]).

compile(ModulePath, OutDir) ->
    boss_compiler:compile(ModulePath, [{out_dir, OutDir}, {include_dirs, [boss_files:include_dir()]}]).

load_view_lib(Application, OutDir, TranslatorPid) ->
    {ok, HelperDirModule} = compile_view_dir_erlydtl(boss_files:view_html_tags_path(), 
        view_custom_tags_dir_module(Application), OutDir, TranslatorPid),
    {ok, [HelperDirModule]}.

load_view_lib_if_old(Application, TranslatorPid) ->
    HelperDirModule = view_custom_tags_dir_module(Application),
    DirNeedsCompile = case module_is_loaded(HelperDirModule) of
        true ->
            module_older_than(HelperDirModule, lists:map(fun
                        ({File, _CheckSum}) -> File;
                        (File) -> File
                    end, [HelperDirModule:source_dir() | HelperDirModule:dependencies()]));
        false ->
            true
    end,
    case DirNeedsCompile of
        true ->
            load_view_lib(Application, undefined, TranslatorPid);
        false ->
            {ok, [HelperDirModule]}
    end.

load_views(Application, OutDir, TranslatorPid) ->
    ModuleList = lists:foldr(fun(Path, Acc) -> 
                {ok, Module} = compile_view_erlydtl(Application, Path, OutDir, TranslatorPid),
                [Module|Acc]
        end, [], boss_files:view_file_list()),
    {ok, ModuleList}.

load_view_if_old(Application, ViewPath, Module, TranslatorPid) ->
    case load_view_lib_if_old(Application, TranslatorPid) of
        {ok, _} -> 
            NeedCompile = case module_is_loaded(Module) of
                true ->
                    Dependencies = lists:map(fun
                            ({File, _CheckSum}) -> File;
                            (File) -> File
                        end, [Module:source() | Module:dependencies()]),
                    TagHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_tag_helper_list()),
                    FilterHelpers = lists:map(fun erlang:list_to_atom/1, boss_files:view_filter_helper_list()),
                    ExtraTagHelpers = boss_env:get_env(template_tag_modules, []),
                    ExtraFilterHelpers = boss_env:get_env(template_filter_modules, []),
                    module_older_than(Module, 
                        Dependencies ++ TagHelpers ++ FilterHelpers ++ ExtraTagHelpers ++ ExtraFilterHelpers);
                false ->
                    true
            end,
            case NeedCompile of
                true ->
                    compile_view_erlydtl(Application, ViewPath, undefined, TranslatorPid);
                false ->
                    {ok, Module}
            end;
        Err ->
            Err
    end.

load_view_if_dev(Application, ViewPath, TranslatorPid) ->
    Module = view_module(Application, ViewPath),
    Result = case boss_env:is_developing_app(Application) of
        true -> load_view_if_old(Application, ViewPath, Module, TranslatorPid);
        false -> {ok, Module}
    end,
    case Result of
        {ok, Module} ->
            case code:ensure_loaded(Module) of
                {module, Module} -> {ok, Module};
                _ -> {error, not_found}
            end;
        Other ->
            Other
    end.

module_is_loaded(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            true;
        _ ->
            false
    end.

module_older_than(Module, Files) when is_atom(Module) ->
    case code:is_loaded(Module) of
        {file, Loaded} ->
            module_older_than(Loaded, Files);
        _ ->
            case code:load_file(Module) of
                {module, _} ->
                    case code:is_loaded(Module) of
                        {file, Loaded} ->
                            module_older_than(Loaded, Files)
                    end;
                {error, _} ->
                    true
            end
    end;
module_older_than(Module, Files) when is_list(Module) ->
    case filelib:last_modified(Module) of
        0 ->
            true;
        CompileDate ->
            module_older_than(CompileDate, Files)
    end;
module_older_than(_Date, []) ->
    false;
module_older_than(CompileDate, [File|Rest]) when is_list(File) ->
    module_older_than(CompileDate, [filelib:last_modified(File)|Rest]);
module_older_than(CompileDate, [Module|Rest]) when is_atom(Module) ->
    {file, Loaded} = code:is_loaded(Module),
    module_older_than(CompileDate, [Loaded|Rest]);
module_older_than(CompileDate, [CompareDate|Rest]) ->
    CompileSeconds = calendar:datetime_to_gregorian_seconds(CompileDate),
    ModificationSeconds = calendar:datetime_to_gregorian_seconds(CompareDate),
    (ModificationSeconds >= CompileSeconds) orelse module_older_than(CompileDate, Rest).

view_module(Application, RelativePath) ->
    Components = tl(filename:split(RelativePath)),
    Lc = string:to_lower(lists:concat([Application, "_", string:join(Components, "_")])),
    ModuleIOList = re:replace(Lc, "\\.", "_", [global]),
    list_to_atom(binary_to_list(iolist_to_binary(ModuleIOList))).

view_custom_tags_dir_module(Application) ->
    list_to_atom(lists:concat([Application, ?CUSTOM_TAGS_DIR_MODULE])).

incoming_mail_controller_module(Application) ->
    list_to_atom(lists:concat([Application, "_incoming_mail_controller"])).

vcs_vsn_cmd(git) ->
    case os:type() of
        {win32,nt} ->
            "FOR /F \"usebackq tokens=* delims=\" %i in "
            "(`git log -n 1 \"--pretty=format:%h\" .`) do "
            "@git describe --always --tags %i";
        _ ->
            "git describe --always --tags "
            "`git log -n 1 --pretty=format:%h .`"
    end;
vcs_vsn_cmd(hg)  -> "hg identify -i";
vcs_vsn_cmd(bzr) -> "bzr revno";
vcs_vsn_cmd(svn) -> "svnversion";
vcs_vsn_cmd({cmd, _Cmd}=Custom) -> Custom;
vcs_vsn_cmd(Version) -> {unknown, Version}.

