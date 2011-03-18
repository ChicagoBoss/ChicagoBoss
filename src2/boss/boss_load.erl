-module(boss_load).
-compile(export_all).
-define(HELPER_MODULE_NAME, '__boss_helper_module').

load_all_modules() ->
    load_all_modules(undefined).

load_all_modules(OutDir) ->
    load_dirs(boss_files:test_path(), OutDir, fun compile/2),
    load_libraries(OutDir),
    load_mail_controllers(OutDir),
    load_web_controllers(OutDir),
    load_models(OutDir),
    load_view_lib(OutDir),
    load_views(OutDir).

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
    load_dirs(boss_files:web_controller_path(), OutDir, fun compile/2).

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
            Module = filename:basename(File, ".erl"),
            AbsPath = filename:join([Dir, File]),
            case OutDir of
                undefined ->
                    case module_older_than(list_to_atom(Module), [AbsPath]) of
                        true ->
                            case Compiler(AbsPath, OutDir) of
                                ok ->
                                    {ok, Module};
                                Err ->
                                    Err
                            end;
                        _ ->
                            {ok, Module}
                    end;
                _ ->
                    case Compiler(AbsPath, OutDir) of
                        ok ->
                            {ok, Module};
                        Err ->
                            Err
                    end
            end;
        _ ->
            ok
    end.

view_doc_root(ViewPath) ->
    filename:join(lists:reverse(lists:nthtail(2, lists:reverse(filename:split(ViewPath))))).

compile_view_dir_erlydtl(LibPath, OutDir) ->
    erlydtl_compiler:compile_dir(LibPath, ?HELPER_MODULE_NAME,
        [{doc_root, filename:join(lists:reverse(lists:nthtail(1, lists:reverse(filename:split(LibPath)))))},
            {compiler_options, []}, {out_dir, OutDir}]).

compile_view_erlydtl(ViewPath, OutDir) ->
    erlydtl_compiler:compile(ViewPath, view_module(ViewPath),
        [{doc_root, view_doc_root(ViewPath)}, {custom_tags_module, ?HELPER_MODULE_NAME},
            {compiler_options, []}, {out_dir, OutDir}]).

compile_model(ModulePath, OutDir) ->
    boss_record_compiler:compile(ModulePath, [{out_dir, OutDir}]).

compile(ModulePath, OutDir) ->
    boss_compiler:compile(ModulePath, [{out_dir, OutDir}]).

load_view_lib() ->
    load_view_lib(undefined).
load_view_lib(OutDir) ->
    compile_view_dir_erlydtl(boss_files:view_lib_path(), OutDir).

load_view_lib_if_old(ViewLibPath, Module) ->
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
            case compile_view_dir_erlydtl(ViewLibPath, undefined) of
                ok -> {ok, Module};
                Err -> Err
            end;
        false ->
            {ok, Module}
    end.

load_views() ->
    load_views(undefined).
load_views(OutDir) ->
    lists:map(fun(Path) -> compile_view_erlydtl(Path, OutDir) end, boss_files:view_file_list()).

load_view_if_old(ViewPath, Module) ->
    case load_view_lib_if_old(boss_files:view_lib_path(), ?HELPER_MODULE_NAME) of
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
                    case compile_view_erlydtl(ViewPath, undefined) of
                        ok -> {ok, Module};
                        Err -> Err
                    end;
                false ->
                    {ok, Module}
            end;
        Err ->
            Err
    end.

load_view_if_dev(ViewPath) ->
    Module = view_module(ViewPath),
    case boss_env() of
        production -> {ok, Module};
        testing -> {ok, Module};
        _ -> load_view_if_old(ViewPath, Module)
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

view_module(ViewPath) ->
    [File, Folder, Type|_] = lists:reverse(filename:split(ViewPath)),
    Lc = string:to_lower(Type ++ "_" ++ Folder ++ "_" ++ File),
    ModuleIOList = re:replace(Lc, "\\.", "_", [global]),
    list_to_atom(binary_to_list(iolist_to_binary(ModuleIOList))).

boss_env() ->
    case get(boss_environment) of
        undefined -> setup_boss_env();
        Val -> Val
    end.

setup_boss_env() ->	
    case boss_load:module_is_loaded(reloader) of
        true -> put(boss_environment, development), development;
        false -> put(boss_environment, production), production
    end.			
