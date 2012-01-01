-module(boss_load).
-compile(export_all).
-define(HELPER_MODULE_NAME, '_view_lib').

load_all_modules(Application, TranslatorPid) ->
    load_all_modules(Application, TranslatorPid, undefined).

load_all_modules(Application, TranslatorPid, OutDir) ->
    {ok, TestModules} = load_dirs(boss_files:test_path(), OutDir, fun compile/2),
    {ok, LibModules} = load_libraries(OutDir),
    {ok, MailModules} = load_mail_controllers(OutDir),
    {ok, ControllerModules} = load_web_controllers(OutDir),
    {ok, ModelModules} = load_models(OutDir),
    {ok, ViewLibModule} = load_view_lib(Application, OutDir, TranslatorPid),
    {ok, ViewModules} = load_views(Application, OutDir, TranslatorPid),
    AllModules = [{test_modules, TestModules}, {lib_modules, LibModules},
        {mail_modules, MailModules}, {controller_modules, ControllerModules},
        {model_modules, ModelModules}, {view_lib_modules, [ViewLibModule]},
        {view_modules, ViewModules}],
    {ok, AllModules}.

load_all_modules_and_emit_app_file(AppName, OutDir) ->
    TranslatorPid = boss_translator:start([{application, AppName}]),
    {ok, ModulePropList} = load_all_modules(AppName, TranslatorPid, OutDir),
    AllModules = lists:foldr(fun({_, Mods}, Acc) -> Mods ++ Acc end, [], ModulePropList),
    DotAppSrc = lists:concat([AppName, ".app.src"]),
    {ok, [{application, AppName, AppData}]} = file:consult(DotAppSrc),
    AppData1 = lists:keyreplace(modules, 1, AppData, {modules, AllModules}),
    DefaultEnv = proplists:get_value(env, AppData1, []),
    AppData2 = lists:keyreplace(env, 1, AppData1, {env, ModulePropList ++ DefaultEnv}),

    IOList = io_lib:format("~p.~n", [{application, AppName, AppData2}]),
    AppFile = filename:join([OutDir, lists:concat([AppName, ".app"])]),
    file:write_file(AppFile, IOList).

reload_all() ->
    Modules = [M || {M, F} <- code:all_loaded(), is_list(F), not code:is_sticky(M)],
    [begin code:purge(M), code:load_file(M) end || M <- Modules].

load_libraries() ->
    load_libraries(undefined).
load_libraries(OutDir) ->
    load_dirs(boss_files:lib_path(), OutDir, fun compile/2).

load_mail_controllers() ->
    load_mail_controllers(undefined).
load_mail_controllers(OutDir) ->
    load_dirs(boss_files:mail_controller_path(), OutDir, fun compile/2).

load_web_controllers() ->
    load_web_controllers(undefined).
load_web_controllers(OutDir) ->
    load_dirs(boss_files:web_controller_path(), OutDir, fun compile_controller/2).

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
    Res = erlydtl_compiler:compile_dir(LibPath, Module,
        [{doc_root, view_doc_root(LibPath)}, {compiler_options, []}, 
            {out_dir, OutDir}, {custom_tags_modules, [boss_erlydtl_tags]},
            {blocktrans_fun, fun(BlockName, Locale) ->
                    case boss_translator:lookup(TranslatorPid, BlockName, Locale) of
                        undefined -> default;
                        Body -> list_to_binary(Body)
                    end
            end}]),
    case Res of
        ok -> {ok, Module};
        Err -> Err
    end.

compile_view_erlydtl(Application, ViewPath, OutDir, TranslatorPid) ->
    HelperModule = helper_module(Application),
    Module = view_module(Application, ViewPath),
    Res = erlydtl_compiler:compile(ViewPath, Module,
        [{doc_root, view_doc_root(ViewPath)}, {custom_tags_modules, [boss_erlydtl_tags, HelperModule]},
            {compiler_options, []}, {out_dir, OutDir}, {blocktrans_fun,
                fun(BlockName, Locale) ->
                        case boss_translator:lookup(TranslatorPid, BlockName, Locale) of
                            undefined -> default;
                            Body -> list_to_binary(Body)
                        end
                end}, {blocktrans_locales, boss_files:language_list()}]),
    case Res of
        ok -> {ok, Module};
        Err -> Err
    end.

compile_model(ModulePath, OutDir) ->
    boss_record_compiler:compile(ModulePath, [{out_dir, OutDir}]).

compile_controller(ModulePath, OutDir) ->
    boss_controller_compiler:compile(ModulePath, [{out_dir, OutDir}]).

compile(ModulePath, OutDir) ->
    boss_compiler:compile(ModulePath, [{out_dir, OutDir}]).

load_view_lib(Application, OutDir, TranslatorPid) ->
    HelperModule = helper_module(Application),
    compile_view_dir_erlydtl(boss_files:view_lib_path(), HelperModule, OutDir, TranslatorPid).

load_view_lib_if_old(ViewLibPath, Module, TranslatorPid) ->
    NeedCompile = case module_is_loaded(Module) of
        true ->
            module_older_than(Module, lists:map(fun
                        ({File, _CheckSum}) -> File;
                        (File) -> File
                    end, [Module:source_dir() | Module:dependencies()]));
        false ->
            true
    end,
    case NeedCompile of
        true ->
            compile_view_dir_erlydtl(ViewLibPath, Module, undefined, TranslatorPid);
        false ->
            {ok, Module}
    end.

load_views(Application, OutDir, TranslatorPid) ->
    ModuleList = lists:foldr(fun(Path, Acc) -> 
                {ok, Module} = compile_view_erlydtl(Application, Path, OutDir, TranslatorPid),
                [Module|Acc]
        end, [], boss_files:view_file_list()),
    {ok, ModuleList}.

load_view_if_old(Application, ViewPath, Module, TranslatorPid) ->
    HelperModule = helper_module(Application),
    case load_view_lib_if_old(boss_files:view_lib_path(), HelperModule, TranslatorPid) of
        {ok, _} -> 
            NeedCompile = case module_is_loaded(Module) of
                true ->
                    module_older_than(Module, lists:map(fun
                                ({File, _CheckSum}) -> File;
                                (File) -> File
                            end, [Module:source() | Module:dependencies()]));
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
module_older_than(CompileDate, [File|Rest]) ->
    CompileSeconds = calendar:datetime_to_gregorian_seconds(CompileDate),
    ModificationSeconds = calendar:datetime_to_gregorian_seconds(
        filelib:last_modified(File)),
    (ModificationSeconds >= CompileSeconds) orelse module_older_than(CompileDate, Rest).

view_module(Application, RelativePath) ->
    Components = tl(filename:split(RelativePath)),
    Lc = string:to_lower(lists:concat([Application, "_", string:join(Components, "_")])),
    ModuleIOList = re:replace(Lc, "\\.", "_", [global]),
    list_to_atom(binary_to_list(iolist_to_binary(ModuleIOList))).

helper_module(Application) ->
    list_to_atom(lists:concat([Application, ?HELPER_MODULE_NAME])).

incoming_mail_controller_module(Application) ->
    list_to_atom(atom_to_list(Application) ++ "_incoming_mail_controller").
