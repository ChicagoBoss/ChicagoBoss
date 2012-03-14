-module(boss_files).
-export([
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
        static_path/1,
        test_list/0,
        test_path/0,
        view_file_list/0,
        view_helpers_path/0,
        view_html_tags_path/0,
        view_filter_helper_list/0,
        view_tag_helper_list/0,
        web_controller/2,
        web_controller_list/1,
        web_controller_path/0,
        web_view_path/0,
        web_view_path/2]).

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
web_view_path(Controller, Template) -> web_view_path(Controller, Template, "html").
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

view_lib_path() -> filename:join([root_src_dir(), "view", "lib"]).

view_tag_helper_path() -> filename:join([view_lib_path(), "tag_modules"]).

view_filter_helper_path() -> filename:join([view_lib_path(), "filter_modules"]).

view_html_tags_path() -> filename:join([view_lib_path(), "tag_html"]).

view_helpers_path() -> [view_tag_helper_path(), view_filter_helper_path()].

view_tag_helper_list() -> module_list([view_tag_helper_path()]).

view_filter_helper_list() -> module_list([view_filter_helper_path()]).

web_controller_path() -> [filename:join([root_src_dir(), "controller"])].

mail_controller_path() -> [filename:join([root_src_dir(), "mail"])].

test_path() -> [filename:join([root_src_dir(), "test", "functional"])].

ebin_dir() -> filename:join([root_dir(), "ebin"]).

include_dir() -> filename:join([root_dir(), "include"]).

test_list() ->
    module_list(test_path()).

model_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            module_list(model_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, model_modules, []))
    end.

web_controller_list(AppName) ->
    case boss_env:is_developing_app(AppName) of
        true ->
            module_list(web_controller_path());
        false ->
            lists:map(fun atom_to_list/1, boss_env:get_env(AppName, controller_modules, []))
    end.

web_controller(AppName, Controller) ->
    lists:concat([AppName, "_", Controller, "_controller"]).

view_file_list() ->
    ViewFiles = filelib:fold_files(filename:join([root_src_dir(), "view"]), ".*\\.(html|txt)$", true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
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

module_list(Dirs) ->
    module_list1(Dirs, []).

module_list1([], ModuleAcc) ->
    lists:sort(ModuleAcc);
module_list1([Dir|Rest], ModuleAcc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Modules = lists:map(fun(X) -> filename:basename(X, ".erl") end,
                lists:filter(fun
                        ("."++_) ->
                            false;
                        (File) -> lists:suffix(".erl", File)
                    end, Files)),
            module_list1(Rest, Modules ++ ModuleAcc);
        _ ->
            module_list1(Rest, ModuleAcc)
    end.

dot_app_src(AppName) ->
	filename:join(["src", lists:concat([AppName, ".app.src"])]).
