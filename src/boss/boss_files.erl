-module(boss_files).
-compile(export_all).

root_dir() -> filename:join([filename:dirname(code:which(?MODULE)), ".."]).

view_path() -> filename:join([root_dir(), "View"]).
view_path(Controller) -> filename:join([view_path(), Controller]).
view_path(Controller, Template) -> filename:join([view_path(Controller), lists:concat([Template, ".html"])]).

model_path() -> filename:join([root_dir(), "Model"]).
model_path(Model) -> filename:join([model_path(), Model]).

controller_path() -> filename:join([root_dir(), "Controller"]).

lang_path() -> filename:join([root_dir(), "Lang"]).
lang_path(Lang) -> filename:join([lang_path(), lists:concat(["strings.", Lang, ".po"])]).

ebin_dir() -> filename:join([root_dir(), "ebin"]).

test_path() -> filename:join([root_dir(), "Test"]).

test_list() ->
    module_list(test_path()).

model_list() ->
    module_list(model_path()).

view_file_list() ->
    Pattern = filename:join([view_path(), "*", "*.html"]),
    filelib:wildcard(Pattern).

language_list() ->
    {ok, Files} = file:list_dir(lang_path()),
    lists:sort(lists:map(fun("strings."++Lang) -> filename:basename(Lang, ".po") end,
            lists:filter(fun
                    ("strings."++_Lang) -> true;
                    (_) -> false
                end, Files))).

module_list(Path) ->
    {ok, Files} = file:list_dir(Path),
    lists:sort(lists:map(fun(X) -> filename:basename(X, ".erl") end,
            lists:filter(fun
                    ("."++_) ->
                        false;
                    (File) -> lists:suffix(".erl", File)
                end, Files))).
