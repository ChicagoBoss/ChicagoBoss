-module(boss_load_lib).
-author('chan.sisowath@gmail.com').

-export([compile/4]).
-export([compile_cbapp/3]).

compile(Type, App, Path, CBApps) -> 
    case lists:member(App, CBApps) of
        true  -> 
            %lager:info("true Type ~p, ~p, ~p",[Type, App, Path]),
            compile_cbapp(Type, App, Path);
        false -> 
            compile_app(Type, App, Path)
    end.
                     
%% ------------------------------------------------------------------------------    
%% Compile Other
%% ------------------------------------------------------------------------------    

%% todo: maybe should not use aleppo here to 
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
%% TEST FUNTINAL
compile_cbapp(top, App, Path = ["src", "test", "functional"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [test] ~p", [CompileResult]);

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
    lager:notice("top [model] ~p", [CompileResult]);

%% MAIL CONTROLLER
compile_cbapp(top, App, Path = ["src", "mail"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [mail] ~p", [CompileResult]);

%% LIBRARY
compile_cbapp(top, App, Path = ["src", "lib"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [lib] ~p", [CompileResult]);

%% WEBSOCKET
compile_cbapp(top, App, Path = ["src", "websocket"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [websocket] Compile Result ~p", [CompileResult]);

%% VIEW LIB HELPER, FILTER
compile_cbapp(top, App, Path = ["src", "view", "lib", "filter_modules"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [view filter] ~p", [CompileResult]);

%% VIEW LIB HELPER, TAG
compile_cbapp(top, App, Path = ["src", "view", "lib", "tag_modules"| _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("top [view tag] ~p", [CompileResult]);

%% VIEW LIB TAG_HTML
compile_cbapp(top, App, Path = ["src", "view", "lib", "tag_html"| _]) when is_list(App)->
    compile_cbapp(top, list_to_atom(App), Path);
compile_cbapp(top, App, _Path = ["src", "view", "lib", "tag_html"| _]) ->
    OutDir = "ebin",
    TranslatorPid = boss_web:translator_pid(App),
    CompileResult = boss_load:compile_view_dir_erlydtl(App,
                                                       boss_files_util:view_html_tags_path(), 
                                                       boss_load:view_custom_tags_dir_module(App),
                                                       OutDir, 
                                                       TranslatorPid),
    lager:notice("top [tag_html] ~p", [CompileResult]);


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
    lager:notice("top [view] ~p", [CompileResult]);
        

                            %% ------------------- %%


%% IN APPS/DEPS FOLDER
%% TEST FUNCTIONAL
compile_cbapp(Type, App, Path = ["src", "test", "functional"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [test] ~p", [Type, CompileResult]);

%% CONTROLLER
compile_cbapp(Type, App, Path = ["src", "controller", _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_controller/2),
    lager:notice("~p [controller] ~p", [Type, CompileResult]);

%% MODEL
compile_cbapp(Type, App, Path = ["src", "model"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile_model/2),
    lager:notice("~p [model] ~p", [Type, CompileResult]);

%% LIRARY
compile_cbapp(Type, App, Path = ["src", "lib"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [lib] ~p", [Type, CompileResult]);

%% MAIL CONTROLLER
compile_cbapp(Type, App, Path = ["src", "mail"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [mail] ~p", [Type, CompileResult]);

%% WEBSOCKET
compile_cbapp(Type, App, Path = ["src", "websocket"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [websocket] ~p", [Type, CompileResult]);

%% VIEW LIB HELPER, FILER
compile_cbapp(Type, App, Path = ["src", "view", "lib", "filter_modules"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [view filter] ~p", [Type, CompileResult]);

%% VIEW LIB HELPER, TAG
compile_cbapp(Type, App, Path = ["src", "view", "lib", "tag_modules"| _]) ->
    Folder = atom_to_list(Type),
    Filename = filename:join([Folder, App] ++ Path),
    OutDir = filename:join([Folder, App, "ebin"]),
    CompileResult = boss_load:maybe_compile(Filename, 
                                            list_to_atom(App), 
                                            OutDir, 
                                            fun boss_load:compile/2),
    lager:notice("~p [view tag] ~p", [Type, CompileResult]);

%% VIEW LIB TAG_HTML
compile_cbapp(Type, App, Path = ["src", "view", "lib", "tag_html"| _]) when is_list(App)->
    compile_cbapp(top, list_to_atom(App), Path);
compile_cbapp(Type, App, _Path = ["src", "view", "lib", "tag_html"| _]) ->
    Folder = atom_to_list(Type),
    OutDir = filename:join([Folder, App, "ebin"]),
    TranslatorPid = boss_web:translator_pid(App),
    CompileResult = boss_load:compile_view_dir_erlydtl(App,
                                                       boss_files_util:view_html_tags_path(), 
                                                       boss_load:view_custom_tags_dir_module(App),
                                                       OutDir, 
                                                       TranslatorPid),
    lager:notice("top [vieaw tag_html] ~p", [CompileResult]);

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
    %% lager:info("### App ~p",[App]),
    %% lager:info("### Filename ~p",[Filename]),
    %% lager:info("### TplAdapter ~p",[TplAdapter]),
    %% lager:info("### OutDir ~p",[OutDir]),
    %% lager:info("### TranslatorPid ~p",[TranslatorPid]),
    CompileResult = boss_load:compile_view(App, Filename, TplAdapter, OutDir, TranslatorPid),
    lager:notice("deps [view] ~p", [CompileResult]);

%% RELOAD PO FILE 
compile_cbapp(_Type, App, ["priv", "lang" | _] = Path) -> 
    Last = lists:last(Path),
    case filename:extension(Last) of
        ".po" -> 
            ["strings", Locale, "po"] = string:tokens(Last, "."),
            File = filename:join([App | Path]),
            boss_web:reload_translation(App, Locale);
        _ -> skip
    end;

compile_cbapp(_Type, _App, _Path) -> 
    lager:info("boss_load ignore ~p", [filename:join([_App |_Path])]),
    ignore.





