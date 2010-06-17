-module(boss_load).
-compile(export_all).

load_all_modules() ->
    load_dir(boss_files:test_path(), fun compile_controller/1),
    load_mail_controllers(),
    load_web_controllers(),
    load_models(),
    load_views().

load_mail_controllers() ->
    load_dir(boss_files:mail_controller_path(), fun compile_controller/1).

load_web_controllers() ->
    load_dir(boss_files:web_controller_path(), fun compile_controller/1).

load_models() ->
    load_dir(boss_files:model_path(), fun compile_model/1).

load_dir(Dir, Compiler) ->
    {ok, Files} = file:list_dir(Dir),
    {ModuleList, ErrorList} = lists:foldl(fun
            ("."++_, Acc) ->
                Acc;
            (File, {Modules, Errors}) ->
                case lists:suffix(".erl", File) of
                    true ->
                        Module = filename:basename(File, ".erl"),
                        AbsPath = filename:join([Dir, File]),
                        case module_older_than(list_to_atom(Module), [AbsPath]) of
                            true ->
                                case Compiler(AbsPath) of
                                    ok ->
                                        {[Module|Modules], Errors};
                                    {error, Error} ->
                                        {Modules, [Error | Errors]};
                                    {error, NewErrors, _NewWarnings} when is_list(NewErrors) ->
                                        {Modules, NewErrors ++ Errors}
                                end;
                            _ ->
                                {[Module|Modules], Errors}
                        end;
                    _ ->
                        {Modules, Errors}
                end
        end, {[], []}, Files),
    case length(ErrorList) of
        0 ->
            {ok, ModuleList};
        _ ->
            {error, ErrorList}
    end.

view_doc_root(ViewPath) ->
    filename:join(lists:reverse(lists:nthtail(2, lists:reverse(filename:split(ViewPath))))).

compile_view_erlydtl(ViewPath) ->
    erlydtl_compiler:compile(ViewPath, view_module(ViewPath),
        [{doc_root, view_doc_root(ViewPath)}, {compiler_options, []}]).

compile_model(ModulePath) ->
    boss_record_compiler:compile(ModulePath).

load_views() ->
    lists:map(fun compile_view_erlydtl/1, boss_files:view_file_list()).

compile_controller(ModulePath) ->
    boss_controller_compiler:compile(ModulePath).

load_view_if_old(ViewPath, Module) ->
    NeedCompile = case module_is_loaded(Module) of
        true ->
            module_older_than(Module, lists:map(fun
                        ({File, _CheckSum}) -> 
                            File;
                        (File) ->
                            File
                    end, [Module:source() | Module:dependencies()]));
        false ->
            true
    end,
    case NeedCompile of
        true ->
            case compile_view_erlydtl(ViewPath) of
                ok ->
                    {ok, Module};
                Err ->
                    Err
            end;
        false ->
            {ok, Module}
    end.

load_view_if_dev(ViewPath) ->
    Module = view_module(ViewPath),
    case get(boss_environment) of
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

compile_forms(Forms, File, Options) ->
    case compile:forms(Forms, Options) of
        {ok, Module1, Bin} ->
            code:purge(Module1),
            case code:load_binary(Module1, File, Bin) of
                {module, _} -> {ok, Module1, Bin};
                _ -> {error, lists:concat(["code reload failed: ", Module1])}
            end;
        OtherError ->
            OtherError
    end.
