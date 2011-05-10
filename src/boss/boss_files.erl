-module(boss_files).
-compile(export_all).

root_dir() -> filename:absname(""). %filename:join([filename:dirname(code:which(?MODULE)), ".."]).
root_admin_dir() -> filename:join([root_dir(), "admin"]).

web_view_path("admin") -> 
    filename:join([root_admin_dir(), "view"]);
web_view_path(Controller) -> 
    filename:join([root_dir(), "view", Controller]).
web_view_path(Controller, Template) -> web_view_path(Controller, Template, "html").
web_view_path(Controller, Template, Extension) -> 
    filename:join([web_view_path(Controller), lists:concat([Template, ".", Extension])]).

mail_view_path(Template) -> mail_view_path(Template, "txt").
mail_view_path(Template, Extension) -> 
    filename:join([root_dir(), "mail", "view", lists:concat([Template, ".", Extension])]).

model_path() -> [filename:join([root_dir(), "model"])].
model_path(Model) -> filename:join([hd(model_path()), Model]).

lang_path() -> filename:join([root_dir(), "lang"]).
lang_path(Lang) -> filename:join([lang_path(), lists:concat(["strings.", Lang, ".po"])]).

static_path() -> filename:join([root_dir(), "static"]).
admin_static_path() -> filename:join([root_admin_dir(), "static"]).

lib_path() -> [filename:join([root_dir(), "lib"])].

view_lib_path() -> filename:join([root_dir(), "view", "lib"]).

web_controller_path() -> [filename:join([root_dir(), "controller"]), filename:join([root_admin_dir()])].

mail_controller_path() -> [filename:join([root_dir(), "mail"])].

test_path() -> [filename:join([root_dir(), "test"]), filename:join([root_admin_dir(), "test"])].

ebin_dir() -> filename:join([root_dir(), "ebin"]).

test_list() ->
    module_list(test_path()).

model_list() ->
    module_list(model_path()).

web_controller_list() ->
    module_list(web_controller_path()).

view_file_list() ->
	ViewFiles = filelib:fold_files(filename:join([root_dir(), "view"]), ".*\\.(html|txt)$", true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    MailPattern = filename:join([root_dir(), "mail", "view", "*.{html,txt}"]),
    AdminPattern = filename:join([root_admin_dir(), "view", "*.{html,txt}"]),	
    ViewFiles ++ filelib:wildcard(MailPattern) ++ filelib:wildcard(AdminPattern).

language_list() ->
    {ok, Files} = file:list_dir(lang_path()),
    lists:sort(lists:map(fun("strings."++Lang) -> filename:basename(Lang, ".po") end,
            lists:filter(fun
                    ("strings."++_Lang) -> true;
                    (_) -> false
                end, Files))).

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

routes_file() ->
	filename:join([root_dir(), "boss.routes"]).
