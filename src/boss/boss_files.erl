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

-export([
        root_dir/1
        ,ebin_dir/1
        ,include_dir/1
        ,test_dir/1
        ,model_dir/1
        ,controller_dir/1
        ,view_dir/1
        ,mail_dir/1
        ,websocket_dir/1
        ,lib_dir/1
        ,libview_dir/1
        ,static_dir/1
        ,init_dir/1
        ,rebar_dir/1
        ,priv_dir/1
        ,lang_dir/1
        ,migration_dir/1
        ,view_tag_helper_dir/1
        ,view_filter_helper_dir/1
        ,view_html_tags_dir/1

        ,web_view_path/4

        ,make_extentions/0
        ,module_list/1
        ,adapter_for_extension/2
        ,view_file_list/1
        ,view_tag_helper_list/1
        ,view_filter_helper_list/1
        ]).

-export([make_extentions/1]).
-export([compiler_adapters/0]).
-export([template_adapters/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

-type path()            :: string().
-type app()             :: types:application().
-type tpl_adapter()     :: types:template_adapter().
-type app_name()        :: string().
-type uri_name()        :: string().
-type uri_bin()         :: binary().
-type module_name()     :: string().
-type controller_name() :: string().
-type ws_mapping()      :: {uri_bin(), module()}.
-type file_extension()  :: string(). %% "erl" | "dtl" | "css" ... 
-type po_file()         :: path().
-type locale()          :: string(). %% "en" | "cn" | "it" | "es" ...

-spec root_dir(App)               -> [binary() | path()] when App::app().
-spec test_dir(App)               -> path() when App::app().
-spec ebin_dir(App)               -> path() when App::app().
-spec include_dir(App)            -> path() when App::app().
-spec priv_dir(App)               -> path() when App::app().
-spec model_dir(App)              -> path() when App::app().
-spec controller_dir(App)         -> path() when App::app().
-spec view_dir(App)               -> path() when App::app().
-spec websocket_dir(App)          -> path() when App::app().
-spec mail_dir(App)               -> path() when App::app().
-spec lib_dir(App)                -> path() when App::app().
-spec libview_dir(App)            -> path() when App::app().
-spec static_dir(App)             -> path() when App::app().
-spec init_dir(App)               -> path() when App::app().
-spec rebar_dir(App)              -> path() when App::app().
-spec lang_dir(App)               -> path() when App::app().
-spec migration_dir(App)          -> path() when App::app().
-spec view_tag_helper_dir(App)    -> path() when App::app().
-spec view_filter_helper_dir(App) -> path() when App::app().
-spec view_html_tags_dir(App)     -> path() when App::app().

-spec compiler_adapters() -> ['boss_compiler_adapter_elixir' | 
                             'boss_compiler_adapter_erlang' | 
                             'boss_compiler_adapter_lfe'].

-spec template_adapters() -> ['boss_template_adapter_eex' | 
                              'boss_template_adapter_erlydtl' | 
                              'boss_template_adapter_jade'].

root_dir(App) ->
    case code:priv_dir(App) of
        {error,bad_name} -> 
            %%return absolute path is not an option, 
            %%it mess with a lot of code
            ["."];
        RootDir ->
            [_|Root] = lists:reverse(filename:split(RootDir)),
            lists:reverse(Root)
    end.

test_dir(App) -> 
    filename:join( root_dir(App) ++ [ "test", "functional"]).
ebin_dir(App) when is_atom(App)->
    filename:join( root_dir(App) ++ [ "ebin"]).
include_dir(App) when is_atom(App)->
    filename:join( root_dir(App) ++ [ "include"]).
model_dir(App) when is_atom(App)->
    filename:join( root_dir(App) ++ [ "src", "model"]).
controller_dir(App) when is_atom(App)->
    filename:join( root_dir(App) ++ [ "src", "controller"]).
view_dir(App) ->
    filename:join( root_dir(App) ++ [ "src", "view"]).
mail_dir(App) ->
    filename:join( root_dir(App) ++ [ "src", "mail"]).
websocket_dir(App) ->
    filename:join( root_dir(App) ++ [ "src", "websocket"]).
lib_dir(App) ->
    filename:join( root_dir(App) ++ [ "src", "lib"]).
libview_dir(App) ->
    filename:join( root_dir(App) ++ [ "src", "view", "lib"]).
view_tag_helper_dir(App) ->
    filename:join( root_dir(App) ++ [ "src", "view", "lib", "tag_modules"]).
view_filter_helper_dir(App) -> 
    filename:join( root_dir(App) ++ [ "src", "view", "lib", "filter_modules"]).
view_html_tags_dir(App) ->
    filename:join( root_dir(App) ++ [ "src", "view", "lib", "tag_html"]).

priv_dir(App) ->
    filename:join( root_dir(App) ++ ["priv"]).
static_dir(App) ->
    filename:join( root_dir(App) ++ ["priv", "static"]).
init_dir(App) ->
    filename:join( root_dir(App) ++ ["priv", "init"]).
rebar_dir(App) ->
    filename:join( root_dir(App) ++ ["priv", "rebar"]).
lang_dir(App) ->
    filename:join( root_dir(App) ++ ["priv", "lang"]).
migration_dir(App) ->
    filename:join( root_dir(App) ++ ["priv", "migrations"]).

compiler_adapters() -> 
    [boss_compiler_adapter_erlang, 
     boss_compiler_adapter_elixir, 
     boss_compiler_adapter_lfe].

template_adapters() -> 
    [boss_template_adapter_erlydtl, 
     boss_template_adapter_jade, 
     boss_template_adapter_eex].

-spec module_list(Dir) -> [string()] when Dir::path().
module_list(Dir) when is_list(Dir)->
    CompilerAdapters = compiler_adapters(),
    Extensions = 
        lists:foldl(fun (Adapter, Acc) ->
                            lists:map(fun(Ext) -> "." ++ Ext end, 
                                      Adapter:file_extensions()) ++ Acc
                    end, [], CompilerAdapters),
    Files = find_file(Dir),
    [begin
         hd(string:tokens(lists:last(filename:split(X)), "."))
     end || X <- Files, lists:member(filename:extension(X), Extensions)].

-spec view_tag_helper_list(App) -> [string()] when App::app().
view_tag_helper_list(App) -> 
    module_list(view_tag_helper_dir(App)).

-spec view_filter_helper_list(App) -> [string()] when App::app().
view_filter_helper_list(App) -> 
    module_list(view_filter_helper_dir(App)).

-spec web_view_path(App, Controller, Template, Extension) -> path() when 
      App::app(),
      Controller::controller_name(),
      Template::string(),
      Extension::file_extension().
web_view_path(App, Controller, Template, Extension) -> 
    filename:join([view_dir(App), Controller, lists:concat([Template, ".", Extension])]).

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

-spec websocket_mapping(BaseURL, AppName, Modules) -> [ws_mapping()] 
  when 
      BaseURL::uri_name(), 
      AppName::app_name(), 
      Modules::[module_name()].
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

mail_controller_path() -> 
    [filename:join([boss_files_util:root_src_dir(), "mail"])].

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

-spec web_controller_list(AppName) -> [module_name()] when 
      AppName::app().
web_controller_list(AppName) when is_list(AppName) ->
    web_controller_list(list_to_atom(AppName));

web_controller_list(AppName) ->
    web_controller_list(AppName, boss_env:boss_env()).
web_controller_list(AppName, development) ->
    module_list(controller_dir(AppName));
web_controller_list(AppName, production) ->
    lists:map(fun atom_to_list/1, boss_env:get_env(AppName, controller_modules, [])).

%% web_controller_list(AppName) ->
%%     case boss_env:is_developing_app(AppName) of
%%         true ->
%%             boss_files_util:module_list(AppName, boss_files_util:web_controller_path());
%%         false ->
%%             lists:map(fun atom_to_list/1, boss_env:get_env(AppName, controller_modules, []))
%%     end.

-spec view_module_list(AppName) -> [module_name()] when 
      AppName::app().
view_module_list(AppName) ->
    view_module_list(AppName, boss_env:boss_env()).

%%FIXME??
%%view_module_list(App, development) -> [];
view_module_list(App, development) when is_atom(App)->
    ViewExtensions = ["." ++ X || X <- template_extensions()],
    Dir = view_dir(App),
    Files = find_file(Dir),
    AppName = atom_to_list(App),
    [begin 
         Tokens = filename:split(F),         
         File = case Tokens of
                    ["apps",  AppName, "src" | Rest] ->
                        string:join([AppName | Rest], "_");
                    ["deps", App, "src" | Rest] ->
                        string:join([AppName | Rest], "_");
                    [".", "apps",  AppName, "src" | Rest] ->
                        string:join([AppName | Rest], "_");
                    [".", "deps", AppName, "src" | Rest] ->
                        string:join([AppName | Rest], "_");
                    ["..", AppName, "src" | Rest] ->
                        string:join([AppName | Rest], "_");
                    [".", "src" | Rest] ->
                        string:join([AppName | Rest], "_")                       
                end,
         string:join(string:tokens(File, "."), "_")
     end || F <- Files, lists:member(filename:extension(F), ViewExtensions)];
%%FIXME: helper script to create the boss.config
view_module_list(AppName, _) -> 
    lists:map(fun atom_to_list/1, boss_env:get_env(AppName, view_modules, [])).

-spec view_file_list(App) -> [path()] when App::app().
view_file_list(App) when is_atom(App)->
    ViewExtensions = ["." ++ X || X <- template_extensions()],
    Files = find_file(view_dir(App)),
    [ F || F <- Files, lists:member(filename:extension(F), ViewExtensions)].
    

-spec is_controller_present(AppName, Controller, ModuleList) -> boolean() when
      AppName::app(),
      Controller::controller_name(),
      ModuleList::[module_name()].
is_controller_present(AppName, Controller, ModuleList) ->
    CompilerAdapters = compiler_adapters(),
    lists:foldl(fun(Adapter, false) ->
                ControllerModule = Adapter:controller_module(AppName, Controller),
                lists:member(ControllerModule, ModuleList);
            (_, true) -> true
        end, false, CompilerAdapters).

-spec web_controller(AppName, Controller, ControllerList) -> module() | undefined when
      AppName::app(),
      Controller::controller_name(),
      ControllerList::[module_name()].
web_controller(AppName, Controller, ControllerList) ->
    CompilerAdapters = compiler_adapters(),
    lists:foldl(fun(Adapter, undefined) ->
                ControllerModule = Adapter:controller_module(AppName, Controller),
                case lists:member(ControllerModule, ControllerList) of
                    true -> ControllerModule;
                    false -> undefined
                end;
            (_, Acc) -> Acc
        end, undefined, CompilerAdapters).

-spec compiler_adapter_for_extension(file_extension()) -> tpl_adapter() | undefined.
compiler_adapter_for_extension(("." ++ Extension)) ->
    adapter_for_extension(Extension, compiler_adapters());
compiler_adapter_for_extension(_) -> undefined.

template_adapter_for_extension(("." ++ Extension)) ->
    adapter_for_extension(Extension, template_adapters()).

adapter_for_extension(Extension, Adapters) ->
    lists:foldl(fun(Adapter, undefined) -> 
                        case lists:member(Extension, Adapter:file_extensions()) of
                            true -> Adapter;
                            false -> undefined
                        end;
                   (_, Acc) -> Acc
                end, undefined, Adapters).

-spec template_extensions() -> [file_extension()].
template_extensions() ->
    lists:foldl(fun (Adapter, Acc) -> Acc ++ Adapter:file_extensions() end,
        [], template_adapters()).

%% FIXME: should take App as parameter.
-spec view_file_list() -> [path()].
view_file_list() ->
    ViewExtensions = template_extensions(),
    ViewFilePattern = ".*\\.(" ++ string:join(ViewExtensions, "|") ++ ")$",
    ViewFiles = filelib:fold_files(
                  filename:join([boss_files_util:root_src_dir(), "view"]), 
                  ViewFilePattern,
                  true, 
                  fun(F1,Acc1) -> [F1 | Acc1] end, []),
    MailPattern = filename:join([boss_files_util:root_src_dir(), "mail", "view", "*.{html,txt}"]),
    ViewFiles ++ filelib:wildcard(MailPattern).

-spec init_file_list(App) -> [path()] when
      App::app().
init_file_list(App) ->
    lists:sort(filelib:wildcard(filename:join([root_priv_dir(App), "init", "*.erl"]))).

-spec routes_file(App) -> path() when App::app().
routes_file(App) ->
    filename:join([root_priv_dir(App), lists:concat([App, ".routes"])]).

-spec language_list(App) -> [locale()] when App::app().
%% language_list(App) ->
%%     language_list_dir(boss_files_util:lang_path(App)).
language_list(App) ->
    language_list_dir(boss_files:lang_dir(App)).

language_list_dir(Path) ->
    Files = find_file(Path),
    language_list_dir(Files, []).

language_list_dir([], Acc) -> Acc;
language_list_dir([F|T], Acc) -> 
    Last = lists:last(filename:split(F)),
    case string:tokens(Last, ".") of
        ["strings", Locale, "po"] -> 
            language_list_dir(T, [Locale] ++ Acc);
        _ -> 
            language_list_dir(T, Acc)
    end.

%%FIXME ??
-spec dot_app_src(AppName) -> path() when AppName::app_name().
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
    CompilerAdapters  = compiler_adapters(),
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
		    lookup_module_by_adapater(Application, File, Acc, AdapterVal);
		_ -> []
	    end
    end.

lookup_module_by_adapater(_Application, _File, Acc, undefined) ->
            Acc;
lookup_module_by_adapater(Application, File, Acc, Adapater) ->
    [Adapater:module_name_for_file(Application, File)|Acc].


-spec find_file(Dir) -> [path()] when Dir::path().
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
            find_file(T, Root, [Path] ++ Acc, ModuleAcc);
        true ->
            find_file(T, Root, find_file(Path, ModuleAcc) ++ Acc, ModuleAcc)
    end.
    
	
	

