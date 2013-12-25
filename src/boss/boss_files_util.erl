-module(boss_files_util).

-export([root_dir/0]).
-export([root_src_dir/0]).

-export([web_view_path/0]).

-export([web_view_path/2]).

-export([web_view_path/1]).

-export([web_view_path/3]).

-export([mail_view_path/0]).

-export([mail_view_path/2]).

-export([model_path/0]).

-export([model_path/1]).

-export([lang_path/1]).

-export([static_path/1]).

-export([lang_path/2]).

-export([view_lib_path/0]).

-export([view_tag_helper_path/0]).

-export([view_filter_helper_path/0]).

-export([view_html_tags_path/0]).

-export([view_helpers_path/0]).

-export([module_list/2]).

-export([web_controller_path/0]).

-export([view_tag_helper_list/1]).

-export([test_list/1]).

-export([include_dir/0]).

-export([ebin_dir/0]).

-export([view_filter_helper_list/1]).

-export([web_controller_path/1]).

-export([lib_path/0]).

-export([test_path/0]).

-export([websocket_path/0]).

-export([compiler_adapters/0]).

-export([template_adapters/0]).

-type input_string() :: string().


-spec root_dir() -> input_string().
root_dir() -> filename:absname(""). %filename:join([filename:dirname(code:which(?MODULE)), ".."]).


-spec root_src_dir() -> [99 | 114 | 115,...].
root_src_dir() -> "src".

-spec web_view_path() -> input_string().


web_view_path() ->
    filename:join([root_src_dir(), "view"]).
-spec web_view_path(atom() | binary() | [atom() | [any()] | char()],atom() | string() | number()) -> input_string().
web_view_path(Controller, Template) -> 
    web_view_path(Controller, Template, "html").


-spec web_view_path(atom() | binary() | [atom() | [any()] | char()]) -> input_string().
web_view_path(Controller) -> 
    filename:join([web_view_path(), Controller]).
-spec web_view_path(atom() | binary() | [atom() | [any()] | char()],atom() | string() | number(),atom() | string() | number()) -> input_string().
web_view_path(Controller, Template, Extension) -> 
    filename:join([web_view_path(Controller),
		   lists:concat([Template, ".", Extension])]).
-spec mail_view_path() -> input_string().

mail_view_path() ->
    filename:join([root_src_dir(), "mail", "view"]).


-spec mail_view_path(atom() | string() | number(),atom() | string() | number()) -> input_string().
mail_view_path(Template, Extension) -> 
    filename:join([mail_view_path(), lists:concat([Template, ".", Extension])]).

-spec model_path() -> [input_string(),...].
model_path() -> [filename:join([root_src_dir(), "model"])].

-spec model_path(atom() | binary() | [atom() | [any()] | char()]) -> input_string().
model_path(Model) -> filename:join([hd(model_path()), Model]).
-spec lang_path(_) -> input_string().

lang_path(App) -> filename:join([boss_files:root_priv_dir(App), "lang"]).
-spec static_path(_) -> input_string().

static_path(App) -> filename:join([boss_files:root_priv_dir(App), "static"]).
-spec lang_path(_,atom() | string() | number()) -> input_string().
lang_path(App, Lang) ->
    filename:join([lang_path(App),
		   lists:concat(["strings.", Lang, ".po"])]).
-spec view_lib_path() -> input_string().

view_lib_path() -> filename:join([root_src_dir(), "view", "lib"]).
-spec view_tag_helper_path() -> input_string().

view_tag_helper_path() -> filename:join([view_lib_path(), "tag_modules"]).
-spec view_filter_helper_path() -> input_string().

view_filter_helper_path() -> filename:join([view_lib_path(), "filter_modules"]).
-spec view_html_tags_path() -> input_string().

view_html_tags_path() -> filename:join([view_lib_path(), "tag_html"]).
-spec view_helpers_path() -> [input_string(),...].

view_helpers_path() -> [view_tag_helper_path(), view_filter_helper_path()].
-spec module_list(_,[input_string(),...]) -> [any()].
-spec module_list1([input_string()],_,_) -> [any()].

module_list(Application, Dirs) ->
    module_list1(Dirs, Application, []).

module_list1([], _Application, ModuleAcc) ->
    lists:sort(ModuleAcc);
module_list1([Dir|Rest], Application, ModuleAcc) ->
    CompilerAdapters = compiler_adapters(),
    ExtensionProplist = boss_files:make_extentions(CompilerAdapters),
    ModuleAcc1 = case file:list_dir(Dir) of
                     {ok, Files} ->
                         lists:foldl(
                           fun("."++_, Acc) -> Acc;
                              (File, Acc) ->
                                   %% TODO check is_file/is_dir?
                                   case filename:extension(File) of 
                                       [$.|Extension] ->                                       
                                           case proplists:get_value(Extension, ExtensionProplist) of
                                               undefined -> 
                                                   Acc;
                                               Adapter -> 
                                                   [Adapter:module_name_for_file(Application, File)|Acc]
                                           end;
                                       _ -> []
                                   end
                           end, 
                           ModuleAcc, 
                           Files);

                     _ ->
                         ModuleAcc
                 end,
    module_list1(Rest, Application, ModuleAcc1).
-spec web_controller_path() -> [input_string(),...].

web_controller_path() -> [filename:join([root_src_dir(), "controller"])].
-spec view_tag_helper_list(_) -> [any()].

view_tag_helper_list(AppName) -> module_list(AppName, [view_tag_helper_path()]).
-spec test_list(_) -> [any()].

test_list(AppName) ->
    module_list(AppName, test_path()).
-spec include_dir() -> input_string().

include_dir() -> filename:join([root_dir(), "include"]).
-spec ebin_dir() -> input_string().

ebin_dir() -> filename:join([root_dir(), "ebin"]).
-spec view_filter_helper_list(_) -> [any()].

view_filter_helper_list(AppName) -> module_list(AppName, [view_filter_helper_path()]).
-spec web_controller_path(atom() | binary() | [atom() | [any()] | char()]) -> input_string().

web_controller_path(Controller) -> 
    filename:join([hd(web_controller_path()), Controller]).
-spec lib_path() -> [input_string(),...].

lib_path() -> [filename:join([root_src_dir(), "lib"])].
-spec test_path() -> [input_string(),...].

test_path() -> [filename:join([root_src_dir(), "test", "functional"])].
-spec websocket_path() -> [input_string(),...].

websocket_path() -> [filename:join([root_src_dir(), "websocket"])].
-spec compiler_adapters() -> [types:compiler_adapters()].

compiler_adapters() -> 
    [boss_compiler_adapter_erlang, boss_compiler_adapter_elixir, boss_compiler_adapter_lfe].
-spec template_adapters() -> ['boss_template_adapter_eex' | 'boss_template_adapter_erlydtl' | 'boss_template_adapter_jade',...].

template_adapters() -> [boss_template_adapter_erlydtl, boss_template_adapter_jade, boss_template_adapter_eex].
