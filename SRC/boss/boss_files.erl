-module(boss_files).
-compile(export_all).

root_dir() -> filename:join([filename:dirname(code:which(?MODULE)), ".."]).
root_admin_dir() -> filename:join([root_dir(), "ADMIN"]).

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

model_path() -> [filename:join([root_dir(), "model"]), filename:join([root_admin_dir(), "model"])].
model_path(Model) -> filename:join([hd(model_path()), Model]).

lang_path() -> filename:join([root_dir(), "lang"]).
lang_path(Lang) -> filename:join([lang_path(), lists:concat(["strings.", Lang, ".po"])]).

lib_path() -> [filename:join([root_dir(), "lib"])].

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
    Pattern = filename:join([root_dir(), "{Web,Mail}", "*views", "*.{html,txt}"]),
    AdminPattern = filename:join([root_admin_dir(), "{Web,Mail}", "*views", "*.{html,txt}"]),
    filelib:wildcard(Pattern) ++ filelib:wildcard(AdminPattern).

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
    {ok, Files} = file:list_dir(Dir),
    Modules = lists:map(fun(X) -> filename:basename(X, ".erl") end,
        lists:filter(fun
                ("."++_) ->
                    false;
                (File) -> lists:suffix(".erl", File)
            end, Files)),
    module_list1(Rest, Modules ++ ModuleAcc).
