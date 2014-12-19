-module(boss_load_lib).

-export([compile/4]).

compile(Type, App, Path, CBApps) -> 
    case lists:member(App, CBApps) of
        true  -> 
            compile_cbapp(Type, App, Path);
        false -> 
            compile_app(Type, App, Path)
    end.
                     
%% Compile Other
compile_app(deps, App, Path = ["src" | _]) ->
    Filename = filename:join(["deps", App] ++ Path),
    OutDir   = filename:join(["deps", App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("[~s] ~p", [App, CompileResult]);

compile_app(_, _, _) -> ignore.

    
%% Compile Boss APP
%% TOP FOLLDER CONTROLLER & MODEL
compile_cbapp(top, App, Path = ["src", "controller", _]) ->
    Filename = filename:join(Path),
    OutDir   = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_controller/2),

    lager:notice("top [controller] ~p", [CompileResult]);

compile_cbapp(top, App, Path = ["src", "model"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_model/2),
    lager:notice("top [model] Compile Result ~p", [CompileResult]);

%% SRC/LIB folder
compile_cbapp(top, App, Path = ["src", "lib"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [lib] Compile Result ~p", [CompileResult]);


%% IN APPS/DEPS FOLDER, CONTROLLER & MODEL
compile_cbapp(Type, App, Path = ["src", "controller", _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_controller/2),
    lager:notice("~p [controller] Compile Result ~p", [Type, CompileResult]);

compile_cbapp(Type, App, Path = ["src", "model"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_model/2),
    lager:notice("~p [model] Compile Result ~p", [Type, CompileResult]);

%% SRC/LIB folder
compile_cbapp(Type, App, Path = ["src", "lib"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [lib] Compile Result ~p", [CompileResult]);

compile_cbapp(_Type, _App, _Path) -> 
    lager:info("ignore file ~p", [filename:join([_App |_Path])]),
    ignore.





