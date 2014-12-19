-module(boss_load_lib).

-export([compile/4]).

compile(Type, App, Path, CBApps) -> 
    case lists:member(App, CBApps) of
        true  -> 
            lager:info("true ~p", [{Type, App, Path}]),
            compile_cbapp(Type, App, Path);
        false -> 
            lager:info("false ~p", [{Type, App, Path}]),
            compile_app(Type, App, Path)
    end.
                     
%% Compile Other
compile_app(_Type, _App, _Path) -> skip.
    
%% Compile Boss APP
compile_cbapp(top, App, Path = ["src", "controller", _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, list_to_atom(App), OutDir, fun boss_load:compile_controller/2),
    lager:notice("[controller] Compile Result ~p", [CompileResult]);

compile_cbapp(top, App, Path = ["src", "model", _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    CompileResult = boss_load:maybe_compile(Filename, list_to_atom(App), OutDir, fun boss_load:compile_model/2),
    lager:notice("[model] Compile Result ~p", [CompileResult]).

