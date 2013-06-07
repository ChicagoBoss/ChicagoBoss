-module(boss_files).
-export([
	websocket_path/0,
	websocket_list/1,
	websocket_mapping/3,
        dot_app_src/1,
        ebin_dir/0,
        include_dir/0,
        init_file_list/1,
        lang_path/2,
        language_list/1,
        lib_path/0,
        mail_controller_path/0,
        mail_view_path/0,
        mail_view_path/2,
        model_list/1,
        model_path/0,
        model_path/1,
        routes_file/1,
        root_priv_dir/1,
        static_path/1,
        test_list/1,
        test_path/0,
        view_file_list/0,
        view_helpers_path/0,
        view_html_tags_path/0,
        view_filter_helper_list/1,
        view_tag_helper_list/1,
        view_module_list/1,
        compiler_adapter_for_extension/1,
        template_adapter_for_extension/1,
        template_extensions/0,
        is_controller_present/3,
        web_controller/3,
        web_controller_list/1,
        web_controller_path/0,
        web_view_path/0,
        web_view_path/2,
	 web_view_path/3
    ]).

root_dir() -> filename:absname(""). %filename:join([filename:dirname(code:which(?MODULE)), ".."]).
root_src_dir() -> "src".
root_priv_dir(App) -> 
    case boss_env:is_developing_app(App) of
       true ->
            filename:join([root_dir(), "priv"]);
       false ->
            code:priv_dir(App)
   end.


web_view_path() ->
    filename:join([root_src_dir(), "view"]).
web_view_path(Controller) -> 
    filename:join([web_view_path(), Controller]).
web_view_path(Controller, Template) -> 
    web_view_path(Controller, Template, "html").
web_view_path(Controller, Template, Extension) -> 
    filename:join([web_view_path(Controller), lists:concat([Template, ".", Extension])]).

mail_view_path() ->
    filename:join([root_src_dir(), "mail", "view"]).
mail_view_path(Template, Extension) -> 
    filename:join([mail_view_path(), lists:concat([Template, ".", Extension])]).

model_path() -> [filename:join([root_src_dir(), "model"])].
model_path(Model) -> filename:join([hd(model_path()), Model]).

lang_path(App) -> filename:join([root_priv_dir(App), "lang"]).
lang_path(App, Lang) -> filename:join([lang_path(App), lists:concat(["strings.", Lang, ".po"])]).

static_path(App) -> filename:join([root_priv_dir(App), "static"]).

lib_path() -> [filename:join([root_src_dir(), "lib"])].

websocket_path() -> [filename:join([root_src_dir(), "websocket"])].
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

view_lib_path() -> filename:join([root_src_dir(), "view", "lib"]).

view_tag_helper_path() -> filename:join([view_lib_path(), "tag_modules"]).

view_filter_helper_path() -> filename:join([view_lib_path(), "filter_modules"]).

view_html_tags_path() -> filename:join([view_lib_path(), "tag_html"]).

view_helpers_path() -> [view_tag_helper_path(), view_filter_helper_path()].

view_tag_helper_list(AppName) -> module_list(AppName, [view_tag_helper_path()]).

view_filter_helper_list(AppName) -> module_list(AppName, [view_filter_helper_path()]).

web_controller_path() -> [filename:join([root_src_dir(), "controller"])].

mail_controller_path() -> [filename:join([root_src_dir(), "mail"])].

test_path() -> [filename:join([root_src_dir(), "test", "functional"])].

ebin_dir() -> filename:join([root_dir(), "ebin"]).

include_dir() -> filename:join([root_dir(), "include"]).

test_list(AppName) ->
    module_list(AppName, test_path()).

websocket_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            module_list(AppName, websocket_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, websocket_modules, []))
    end.

model_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            module_list(AppName, model_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, model_modules, []))
    end.

web_controller_list(AppName) when is_list(AppName) ->
    web_controller_list(list_to_atom(AppName));
web_controller_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            module_list(AppName, web_controller_path());
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
    CompilerAdapters = compiler_adapters(),
    lists:foldl(fun(Adapter, false) ->
                ControllerModule = Adapter:controller_module(AppName, Controller),
                lists:member(ControllerModule, ModuleList);
            (_, true) -> true
        end, false, CompilerAdapters).

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

compiler_adapters() -> [boss_compiler_adapter_erlang, boss_compiler_adapter_elixir].

compiler_adapter_for_extension("."++Extension) ->
    adapter_for_extension(Extension, compiler_adapters()).

template_adapters() -> [boss_template_adapter_erlydtl, boss_template_adapter_jade, boss_template_adapter_eex].

template_adapter_for_extension("."++Extension) ->
    adapter_for_extension(Extension, template_adapters()).

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
        [], template_adapters()).

view_file_list() ->
    ViewExtensions = template_extensions(),
    ViewFilePattern = ".*\\.(" ++ string:join(ViewExtensions, "|") ++ ")$",
    ViewFiles = filelib:fold_files(filename:join([root_src_dir(), "view"]), ViewFilePattern, 
        true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    MailPattern = filename:join([root_src_dir(), "mail", "view", "*.{html,txt}"]),
    ViewFiles ++ filelib:wildcard(MailPattern).

init_file_list(App) ->
    lists:sort(filelib:wildcard(filename:join([root_priv_dir(App), "init", "*.erl"]))).

routes_file(App) ->
    filename:join([root_priv_dir(App), lists:concat([App, ".routes"])]).

language_list(App) ->
    language_list_dir(lang_path(App)).

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

module_list(Application, Dirs) ->
    module_list1(Dirs, Application, []).

module_list1([], _Application, ModuleAcc) ->
    lists:sort(ModuleAcc);
module_list1([Dir|Rest], Application, ModuleAcc) ->
    CompilerAdapters = compiler_adapters(),
    ExtensionProplist = lists:foldl(fun
            (Adapter, Acc) ->
                lists:map(fun(Ext) -> {Ext, Adapter} end, Adapter:file_extensions()) ++ Acc
        end, [], CompilerAdapters),
    ModuleAcc1 = case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foldl(fun
                    ("."++_, Acc) -> Acc;
                    (File, Acc) ->
                        [$.|Extension] = filename:extension(File),
                        case proplists:get_value(Extension, ExtensionProplist) of
                            undefined -> Acc;
                            Adapter -> [Adapter:module_name_for_file(Application, File)|Acc]
                        end
                end, ModuleAcc, Files);
        _ ->
            ModuleAcc
    end,
    module_list1(Rest, Application, ModuleAcc1).

dot_app_src(AppName) ->
	filename:join(["src", lists:concat([AppName, ".app.src"])]).

