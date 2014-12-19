-module(boss_load_lib).

-export([compile/4]).

compile(Type, App, Path, CBApps) -> 
    case lists:member(App, CBApps) of
        true  -> 
            compile_cbapp(Type, App, Path);
        false -> 
            compile_app(Type, App, Path)
    end.
                     
%% ------------------------------------------------------------------------------    
%% Compile Other
%% ------------------------------------------------------------------------------    

%% todo: fix here should not use alepo here to 
%%       compile other apps than a cb app
compile_app(deps, App, Path = ["src" | _]) ->
    Filename = filename:join(["deps", App] ++ Path),
    OutDir   = filename:join(["deps", App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("[~s] ~p", [App, CompileResult]);

compile_app(_, _, _) -> ignore.


%% ------------------------------------------------------------------------------    
%% Compile BOSS APP
%% ------------------------------------------------------------------------------    

%% TOP FOLLDER 
%% CONTROLLER & MODEL
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

%% MAIL CONTROLLER
compile_cbapp(top, App, Path = ["src", "mail"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [mail] Compile Result ~p", [CompileResult]);

%% LIBRARY
compile_cbapp(top, App, Path = ["src", "lib"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [lib] Compile Result ~p", [CompileResult]);

%% WEBSOCKET
compile_cbapp(top, App, Path = ["src", "websocket"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [lib] Compile Result ~p", [CompileResult]);

%% VIEW LIB HELPER, FILTER
compile_cbapp(top, App, Path = ["src", "view", "lib", "filter_modules"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [lib] Compile Result ~p", [CompileResult]);

%% VIEW LIB HELPER, TAG
compile_cbapp(top, App, Path = ["src", "view", "lib", "tag_modules"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [lib] Compile Result ~p", [CompileResult]);

%% VIEW LIB TAG_HTML
compile_cbapp(top, App, Path = ["src", "view", "lib", "tag_html"| _]) when is_list(App)->
    compile_cbapp(top, list_to_atom(App), Path);
compile_cbapp(top, App, _Path = ["src", "view", "lib", "tag_html"| _]) ->
    OutDir = "ebin",
    TranslatorPid = boss_web:translator_pi(App),
    CompileResult = boss_load:compile_view_dir_erlydtl(App,
                                                       boss_files_util:view_html_tags_path(), 
                                                       boss_load:view_custom_tags_dir_module(App),
                                                       OutDir, 
                                                       TranslatorPid),
    lager:notice("top [tag_html] Compile Result ~p", [CompileResult]);


%% VIEW
compile_cbapp(Type, App, Path = ["src", "view"| _]) when is_list(App) ->
    compile_cbapp(Type, list_to_atom(App), Path);
compile_cbapp(top, App, Path = ["src", "view"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    TranslatorPid = boss_web:translator_pid(App),    
    TplAdapter = boss_files:template_adapter_for_extension(
                   filename:extension(Filename)),
    CompileResult = boss_load:compile_view(App, Filename, TplAdapter, OutDir, TranslatorPid),
    lager:notice("top [tag_html] Compile Result ~p, ~p", [CompileResult, TranslatorPid]);
        

                            %% ------------------- %%


%% IN APPS/DEPS FOLDER
%% CONTROLLER
compile_cbapp(Type, App, Path = ["src", "controller", _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_controller/2),
    lager:notice("~p [controller] Compile Result ~p", [Type, CompileResult]);

%% MODEL
compile_cbapp(Type, App, Path = ["src", "model"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_model/2),
    lager:notice("~p [model] Compile Result ~p", [Type, CompileResult]);

%% LIRARY
compile_cbapp(Type, App, Path = ["src", "lib"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [lib] Compile Result ~p", [Type, CompileResult]);

%% MAIL CONTROLLER
compile_cbapp(Type, App, Path = ["src", "mail"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [mail] Compile Result ~p", [Type, CompileResult]);

%% WEBSOCKET
compile_cbapp(Type, App, Path = ["src", "websocket"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [mail] Compile Result ~p", [Type, CompileResult]);

%% VIEW LIB HELPER, FILER
compile_cbapp(Type, App, Path = ["src", "view", "lib", "filter_modules"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [mail] Compile Result ~p", [Type, CompileResult]);

%% VIEW LIB HELPER, TAG
compile_cbapp(Type, App, Path = ["src", "view", "lib", "tag_modules"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [mail] Compile Result ~p", [Type, CompileResult]);

%% VIEW LIB TAG_HTML
compile_cbapp(Type, App, Path = ["src", "view", "lib", "tag_html"| _]) when is_list(App)->
    compile_cbapp(top, list_to_atom(App), Path);
compile_cbapp(Type, App, _Path = ["src", "view", "lib", "tag_html"| _]) ->
    Folder = atom_to_list(Type),
    OutDir = filename:join([Folder, App, "ebin"]),
    TranslatorPid = boss_web:translator_pi(App),
    CompileResult = boss_load:compile_view_dir_erlydtl(App,
                                                       boss_files_util:view_html_tags_path(), 
                                                       boss_load:view_custom_tags_dir_module(App),
                                                       OutDir, 
                                                       TranslatorPid),
    lager:notice("top [tag_html] Compile Result ~p", [CompileResult]);

%% VIEW
compile_cbapp(Type, App, Path = ["src", "view"| _]) when is_list(App) ->
    compile_cbapp(Type, list_to_atom(App), Path);
compile_cbapp(Type, App, Path = ["src", "view"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    TranslatorPid = boss_web:translator_pid(App),    
    TplAdapter = boss_files:template_adapter_for_extension(
                   filename:extension(Filename)),
    CompileResult = boss_load:compile_view(App, Filename, TplAdapter, OutDir, TranslatorPid),
    lager:notice("~p [tag_html] Compile Result ~p", [Type, CompileResult]);

compile_cbapp(_Type, _App, _Path) -> 
    lager:info("ignore file ~p", [filename:join([_App |_Path])]),
    ignore.





