-module(boss_files).
-export([
	websocket_list/1,
	websocket_mapping/3,
        dot_app_src/1,
                    init_file_list/1, language_list/1, mail_controller_path/0,
        model_list/1,
        lib_module_list/1,
        routes_file/1,
        root_priv_dir/1, view_file_list/0,
                                                        view_module_list/1,
        compiler_adapter_for_extension/1,
        template_adapter_for_extension/1,
        template_extensions/0,
        is_controller_present/3,
        web_controller/3,
        web_controller_list/1,
        find_file/1
    ]).

-export([make_extentions/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-type input_string() :: string().
-spec root_priv_dir(_) -> input_string().
-spec websocket_mapping(_,_,[any()]) -> any().
-spec mail_controller_path() -> [input_string(),...].
-spec websocket_list(_) -> [any()].
-spec model_list(_) -> [any()].
-spec lib_module_list(_) -> [any()].
-spec web_controller_list(_) -> [any()].
-spec view_module_list(_) -> [string()].
-spec is_controller_present(_,_,_) -> boolean().
-spec web_controller(_,_,_) -> any().
-spec compiler_adapter_for_extension(_) -> any().
-spec template_adapter_for_extension(nonempty_maybe_improper_list()) -> any().
-spec adapter_for_extension(_,['boss_compiler_adapter_elixir' | 'boss_compiler_adapter_erlang' | 'boss_compiler_adapter_lfe' | 'boss_template_adapter_eex' | 'boss_template_adapter_erlydtl' | 'boss_template_adapter_jade',...]) -> any().
-spec template_extensions() -> any().
-spec view_file_list() -> [any()].
-spec init_file_list(_) -> [string()].
-spec routes_file(atom() | string() | number()) -> input_string().
-spec language_list(_) -> [input_string()].
-spec language_list_dir(input_string()) -> [input_string()].
-spec dot_app_src(atom() | string() | number()) -> input_string().
-spec model_list(_,[input_string(),...]) -> [any()].

-spec find_file(input_string()) -> [string()].
-spec find_file(input_string(),[]) -> [string()].
-spec find_file([string()],input_string(),[string()],[]) -> [string()].

root_priv_dir(App) -> 
    Default = filename:join([boss_files_util:root_dir(), "priv"]),
    case boss_env:is_developing_app(App) of
       true -> Default;
       false ->
            case code:priv_dir(App) of
                {error, bad_name} -> Default;
                Dir -> Dir
            end
   end.
websocket_mapping(BaseURL, AppName, Modules) ->
    lists:foldl(fun([], Acc) -> Acc;
		   (M, Acc) -> 
			L1 = string:len(AppName) + 1,
		        L2 = string:len(M),
			L3 = string:len("_websocket"),
			Service = string:substr(M, 
				      L1 + 1, 
				      L2 - (L1+L3)),
			Url = case BaseURL of
				  "/" ->
				      string:join(["/websocket", Service],"/");
				  _ ->
				      string:join([BaseURL, "websocket", Service],"/")
			      end,			
			Acc ++ [{list_to_binary(Url), list_to_atom(M)}]			
		end, [], Modules).

mail_controller_path() -> [filename:join([boss_files_util:root_src_dir(), "mail"])].

websocket_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            boss_files_util:module_list(AppName, boss_files_util:websocket_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, websocket_modules, []))
    end.

model_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            model_list(AppName, boss_files_util:model_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, model_modules, []))
    end.

lib_module_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            boss_files_util:module_list(AppName, boss_files_util:lib_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, lib_modules, []))
    end.

web_controller_list(AppName) when is_list(AppName) ->
    web_controller_list(list_to_atom(AppName));
web_controller_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            boss_files_util:module_list(AppName, boss_files_util:web_controller_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, controller_modules, []))
    end.

view_module_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true -> [];
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, view_modules, []))
    end.

is_controller_present(AppName, Controller, ModuleList) ->
    CompilerAdapters = boss_files_util:compiler_adapters(),
    lists:foldl(fun(Adapter, false) ->
                ControllerModule = Adapter:controller_module(AppName, Controller),
                lists:member(ControllerModule, ModuleList);
            (_, true) -> true
        end, false, CompilerAdapters).

web_controller(AppName, Controller, ControllerList) ->
    CompilerAdapters = boss_files_util:compiler_adapters(),
    lists:foldl(fun(Adapter, undefined) ->
                ControllerModule = Adapter:controller_module(AppName, Controller),
                case lists:member(ControllerModule, ControllerList) of
                    true -> ControllerModule;
                    false -> undefined
                end;
            (_, Acc) -> Acc
        end, undefined, CompilerAdapters).

compiler_adapter_for_extension(("." ++ Extension)) ->
    adapter_for_extension(Extension, boss_files_util:compiler_adapters());
compiler_adapter_for_extension(_) -> undefined.

template_adapter_for_extension(("." ++ Extension)) ->
    adapter_for_extension(Extension, boss_files_util:template_adapters()).

adapter_for_extension(Extension, Adapters) ->
    lists:foldl(fun
            (Adapter, undefined) -> 
                case lists:member(Extension, Adapter:file_extensions()) of
                    true -> Adapter;
                    false -> undefined
                end;
            (_, Acc) -> Acc
        end, undefined, Adapters).

template_extensions() ->
    lists:foldl(fun (Adapter, Acc) -> Acc ++ Adapter:file_extensions() end,
        [], boss_files_util:template_adapters()).

view_file_list() ->
    ViewExtensions = template_extensions(),
    ViewFilePattern = ".*\\.(" ++ string:join(ViewExtensions, "|") ++ ")$",
    ViewFiles = filelib:fold_files(filename:join([boss_files_util:root_src_dir(), "view"]), ViewFilePattern,
        true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    MailPattern = filename:join([boss_files_util:root_src_dir(), "mail", "view", "*.{html,txt}"]),
    ViewFiles ++ filelib:wildcard(MailPattern).

init_file_list(App) ->
    lists:sort(filelib:wildcard(filename:join([root_priv_dir(App), "init", "*.erl"]))).

routes_file(App) ->
    filename:join([root_priv_dir(App), lists:concat([App, ".routes"])]).

language_list(App) ->
    language_list_dir(boss_files_util:lang_path(App)).

language_list_dir(Path) ->
    case file:list_dir(Path) of
        {ok, Files} ->
            lists:sort(lists:map(fun("strings."++Lang) -> filename:basename(Lang, ".po") end,
                    lists:filter(fun
                            ("strings."++_Lang) -> true;
                            (_) -> false
                        end, Files)));
        {error, enoent} ->
            []
    end.

dot_app_src(AppName) ->
	filename:join(["src", lists:concat([AppName, ".app.src"])]).

% add sub folder, ex: 100 models need sub folder
% don't want to change the behavior for other module

model_list(Application, Dirs) ->
    model_list(Dirs, Application, []).


model_list([], _Application, ModuleAcc) ->
    lists:sort(ModuleAcc);
model_list([Dir|Rest], Application, ModuleAcc) ->
    ExtensionProplist = make_extentions(),
    ModuleAcc1        = make_modules(Dir, Application, ModuleAcc, ExtensionProplist),
    model_list(Rest, Application, ModuleAcc1).

make_extentions() ->
    CompilerAdapters  = boss_files_util:compiler_adapters(),
    make_extentions(CompilerAdapters).

    

-spec(make_extentions([types:compiler_adapters()]) ->
	     [{string(), types:compiler_adapters()}]).
make_extentions(CompilerAdapters) ->
    lists:foldl(fun (Adapter, Acc) ->
			lists:map(fun(Ext) -> {Ext, Adapter} end, 
				  Adapter:file_extensions()) ++ Acc
		end, [], CompilerAdapters).


make_modules(Dir, Application, ModuleAcc, ExtensionProplist) ->
    Itter = make_modules_itterator(ExtensionProplist, Application),
    lists:foldl(Itter, ModuleAcc, find_file(Dir, ModuleAcc)).

make_modules_itterator(ExtensionProplist, Application) ->
    fun ("." ++ _, Acc) -> Acc;
	(File, Acc) ->
	    case filename:extension(File) of
		[$. |Extension] ->
		    AdapterVal = proplists:get_value(Extension, ExtensionProplist),
		    lookup_module_by_adapater(Application, File, Acc,
					      AdapterVal);
		_ -> []
	    end
    end.


lookup_module_by_adapater(_Application, _File, Acc, undefined) ->
            Acc;
lookup_module_by_adapater(Application, File, Acc, Adapater) ->
    [Adapater:module_name_for_file(Application, File)|Acc].

find_file(Dir) ->
    find_file(Dir, []).

find_file(Dir, ModuleAcc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->                              
            find_file(Files, Dir, [], ModuleAcc);
        _ -> 
            ModuleAcc
    end.

find_file([], _, Acc, _ModuleAcc) -> Acc;
find_file([H|T], Root, Acc, ModuleAcc) -> 
    Path	= filename:join(Root, H),
    IsDir	= filelib:is_dir(Path),
    case IsDir of
        false ->
            find_file(T, Root, [Path] ++ Acc,                     ModuleAcc);
        true ->
            find_file(T, Root, find_file(Path, ModuleAcc) ++ Acc, ModuleAcc)
    end.
    
	
	

