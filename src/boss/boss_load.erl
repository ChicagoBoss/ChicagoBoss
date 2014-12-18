-module(boss_load).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
        incoming_mail_controller_module/1,
        load_all_modules/2,
        load_all_modules/3,
        load_all_modules_and_emit_app_file/2,
        load_libraries/1,
        load_services_websockets/1,
        load_mail_controllers/1,
        load_models/1,
        load_view_if_dev/4,
        load_view_lib_modules/1,
        load_web_controllers/1,
        module_is_loaded/1,
        reload_all/0
    ]).
-include("boss_web.hrl").

-record(load_state, {last, root}).


-ifdef(TEST).
-compile(export_all).
-endif.

-type module_types() :: [{'controller_modules' | 'lib_modules' |
                          'mail_modules' | 'model_modules' | 'test_modules' |
                          'view_lib_helper_modules' | 'view_lib_tags_modules' | 'view_modules'
                          | 'websocket_modules',maybe_improper_list()},...].


-type reload_error_status_values() :: 'badfile' | 'native_code' | 'nofile' | 'not_purged' | 'on_load' | 'sticky_directory'.
-type application() :: types:application().

     
-spec incoming_mail_controller_module(application()) -> atom().
-spec load_all_modules(application(), atom() | pid() | {atom(),atom()}) ->
   {'ok',module_types()}.
-spec load_all_modules(application(),atom() | pid() | {atom(),atom()},_) ->
   {'ok',module_types()}.
-spec load_all_modules_and_emit_app_file(application(),atom() | binary() | [atom() | [any()] | char()]) -> 
   'ok' | {'error',atom()}.
-spec load_libraries(application()) -> {'error',[any(),...]} | {'ok',[any()]}.
-spec load_mail_controllers(application()) -> {'error',[any(),...]} | {'ok',[any()]}.
-spec load_models(application()) -> {'error',[any(),...]} | {'ok',[any()]}.
-spec load_services_websockets(application()) -> {'error',[any(),...]} | {'ok',[any()]}.
-spec load_view_if_dev(application(), atom() | binary() | [atom() | [any()] | char()],_,_) -> any().
-spec load_view_lib_modules(application()) -> {'error',[any(),...]} | {'ok',[any()]}.
-spec load_web_controllers(application()) -> {'error',[any(),...]} | {'ok',[any()]}.
-spec module_is_loaded(atom()) -> boolean().
-spec reload_all() -> [{'error',reload_error_status_values()}|
		       {'module', atom() | tuple()}].

-define(CUSTOM_TAGS_DIR_MODULE, '_view_lib_tags').

%% code from project github.com/synrc/active 
%%
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Opts) -> 
    error_logger:info_msg("Starting boss_load..."),
    fs:subscribe(), 
    erlang:process_flag(priority, low), 
    {ok, #load_state{last=fresh, root=fs:path()}}.


handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({_Pid, {fs,file_event}, {Path, Flags}}, #load_state{root=Root} = State) ->
    Cur = path_shorten(filename:split(Root)),
    P = filename:split(Path),

    Result = case lists:prefix(Cur, P) of
        true ->
            Components = P -- Cur,
            %%error_logger:info_msg("event: ~p ~p", [Components, Flags]),
            path_event(Components, Flags, State);
        false ->
            ok
    end,

    {noreply, State#load_state{last={event, Path, Flags, Result}}};
handle_info({load_ebin, Atom}, State) -> 
    error_logger:info_msg("load module ~p",[Atom]),
    do_load_ebin(Atom), 
    {noreply, State#load_state{last={do_load_ebin, Atom}}};
handle_info(Info, State) -> {noreply, State#load_state{last={unk, Info}}}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


path_event(C, [E|_Events], _State) when E =:= created; E =:= modified; E =:= renamed ->
    case path_filter(C) of 
        true  -> otp(C); 
        false -> ignore 
    end;
path_event(C, [_E|Events], State) -> 
    path_event(C, Events, State);
path_event(_, [], _State) -> done.

otp(["deps",App|Rest]) -> app(deps, App,Rest);
otp(["apps",App|Rest]) -> app(apps, App,Rest);
otp([Some|Path])       -> app(top, top(),[Some|Path]);
otp(_)                 -> ok.

app(deps, App, ["ebin",Module|_]) -> skip; %%load_ebin(App, Module); %%
app(_, App, ["ebin",Module|_])    -> load_ebin(App, Module);
app(_, App, ["priv","fdlink"++_]) -> skip;
app(_, App, ["priv","mac"++_])    -> skip;
app(_, App, ["priv","windows"++_])-> skip;
app(_, App, ["priv","linux"++_])  -> skip;
app(Type, App, Path=["priv"|_]) -> 
    case hd(lists:reverse(Path)) of
        ".#" ++ _ -> skip; % mc temp files
        Else      -> compile_app(Type, App, Path) 
    end;
app(Type, App,Path=["include"|_])   -> compile_app(Type, App, Path);
app(Type, App,Path=["src"|_])       -> compile_app(Type, App, Path);
app(_, _, _)                        -> ok.

top() -> 
    lists:last(filename:split(filename:absname(""))).

compile_app(deps, App, Path) -> skip;
compile_app(top, App, Path = ["src", "controller", _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    error_logger:info_msg("compile controller module ~p -> ~p",[Filename, OutDir]),
    CompileResult = maybe_compile(Filename, list_to_atom(App), OutDir, fun compile_controller/2),
    error_logger:info_msg("Compile Result ~p", [CompileResult]);

compile_app(top, App, Path = ["src", "model", _]) ->
    Filename = filename:join(Path),
    OutDir = "ebin",
    error_logger:info_msg("compile model module ~p -> ~p",[Filename, OutDir]),
    CompileResult = maybe_compile(Filename, list_to_atom(App), OutDir, fun compile_model/2),
    error_logger:info_msg("Compile Result ~p", [CompileResult]).


%% compile_app(top, App, Path = ["src", "view", _]) ->
%%     Filename = filename:join(Path),
%%     error_logger:info_msg("compile view module ~p",[Filename]),
%%     CompileResult = maybe_compile(Filename, list_to_atom(App), undefined, fun compile_view/2),
%%     error_logger:info_msg("Compile Result ~p", [CompileResult]).

%% compile_app(top, App, Path = ["src", "lib", _]) ->
%%     Filename = filename:join(Path),
%%     error_logger:info_msg("compile lib module ~p",[Filename]),
%%     CompileResult = maybe_compile(Filename, list_to_atom(App), undefined, fun compile_lib/2),
%%     error_logger:info_msg("Compile Result ~p", [CompileResult]).

load_ebin(App,EName) ->
    error_logger:info_msg("load ebin: ~p,~p", [App,EName]),    
    Tokens = string:tokens(EName, "."),
    case Tokens of
        [Name, "beam"] -> do_load_ebin(list_to_atom(Name));
        [Name, "bea#"] ->
            case monitor_handles_renames() of
                false ->
                    erlang:send_after(500, ?SERVER, {load_ebin, list_to_atom(Name)}),
                    delayed;
                true ->
                    ignored
            end;
        %%[Name, Smth] -> ok;
        _ ->
            error_logger:warning_msg("boss_load: unknown BEAM file: ~p", [EName]),
            ok
    end.

do_load_ebin(Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    code:load_binary(Module, Filename, Binary),
    lager:notice("Reloading module: ~p~n\n\r", [Module]),
    reloaded.

monitor_handles_renames([renamed|_]) -> true;
monitor_handles_renames([_|Events]) -> monitor_handles_renames(Events);
monitor_handles_renames([]) -> false.
monitor_handles_renames() ->
    case get(monitor_handles_renames) of
        undefined ->
            R = monitor_handles_renames(fs:known_events()),
            put(monitor_handles_renames, R),
            R;
        V -> V
    end.



%% ["a", "b", ".."] -> ["a"]
path_shorten(Coms) -> path_shorten_r(lists:reverse(Coms), [], 0).

path_shorten_r([".."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count + 1);
path_shorten_r(["."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count);
path_shorten_r([_C|Rest], Acc, Count) when Count > 0 -> path_shorten_r(Rest, Acc, Count - 1);
path_shorten_r([C|Rest], Acc, 0) -> path_shorten_r(Rest, [C|Acc], 0);
path_shorten_r([], Acc, _) -> Acc.

%
% Filters
%

path_filter(L) -> not lists:any(fun(E) -> not path_filter_dir(E) end, L) andalso path_filter_last(lists:last(L)).

path_filter_dir(".git") -> false;
path_filter_dir(".hg")  -> false;
path_filter_dir(".svn") -> false;
path_filter_dir("CVS")  -> false;
path_filter_dir("log")  -> false;
path_filter_dir(_)      -> true.

path_filter_last(".rebarinfo")     -> false;   % new rebars
path_filter_last("LICENSE")        -> false;
path_filter_last("4913 (deleted)") -> false;   % vim magical file
path_filter_last("4913")           -> false;
path_filter_last(_)                -> true.


load_all_modules(Application, TranslatorSupPid) ->
    load_all_modules(Application, TranslatorSupPid, undefined).

load_all_modules(Application, TranslatorSupPid, OutDir) ->
    lager:info("Loading application ~p", [Application]),
    [{_, TranslatorPid, _, _}]	= supervisor:which_children(TranslatorSupPid),
    
    Ops = make_ops_list(TranslatorPid),

    AllModules = make_all_modules(Application, OutDir, Ops),
    {ok, AllModules}.
-type error(X)   :: {ok, X} | {error, string()}.
-type op_key()   :: test_modules|lib_modules|websocket_modules|mail_modules|controller_modules|
                    model_modules| view_lib_tags_modules|view_lib_helper_modules|view_modules.
-type op()       :: {op_key(), fun((atom(), string()) -> error(_))}.
-spec(make_ops_list(pid()) -> [op()]).
make_ops_list(TranslatorPid) ->
    [{test_modules,	       fun load_test_modules/2 		},
     {lib_modules,	       fun load_libraries/2			},
     {websocket_modules,       fun load_services_websockets/2		},
     {mail_modules,	       fun load_mail_controllers/2		},
     {controller_modules,      fun load_web_controllers/2		},
     {model_modules,	       fun load_models/2			},
     {view_lib_helper_modules, fun load_view_lib_modules/2		},
     {view_lib_tags_modules,      load_view_lib(_, _, TranslatorPid)	},
     {view_modules,               load_views(_, _,    TranslatorPid)	}].
    
-spec make_all_modules(atom(), string(), [op()]) -> [{atom(),_}].

make_all_modules(Application, OutDir, Ops) ->
    lists:map(fun({Key, Lambda}) ->
		      case Lambda(Application, OutDir) of
			  {ok, Modules} ->
			      {Key, Modules};
			  {error, Message} ->
			      lager:error("Load Module Error ~p : ~p", [Key, Message]),
			      {Key, []}
		      end
              end, Ops).

load_test_modules(Application, OutDir) ->
    Result = load_dirs(boss_files_util:test_path(),
	      Application,
              OutDir,
              fun compile/2),
    Result.

load_all_modules_and_emit_app_file(AppName, OutDir) ->
    application:start(elixir),
    {ok, TranslatorSupPid}		    = boss_translator:start([{application, AppName}]),
    {ok, ModulePropList}		    = load_all_modules(AppName, TranslatorSupPid, OutDir),
    AllModules				    = lists:foldr(fun({_, Mods}, Acc) -> Mods ++ Acc end, [], ModulePropList),
    DotAppSrc				    = boss_files:dot_app_src(AppName),
    {ok, [{application, AppName, AppData}]} = file:consult(DotAppSrc),
    AppData1				    = lists:keyreplace(modules, 1, AppData, {modules, AllModules}),
    Vsn					    = proplists:get_value(vsn, AppData1, []),
    ComputedVsn                             = make_computed_vsn(Vsn),
    AppData2                                = lists:keyreplace(vsn, 1, AppData1, {vsn, ComputedVsn}),
    DefaultEnv                              = proplists:get_value(env, AppData2, []),
    AppData3                                = lists:keyreplace(env, 1, AppData2, {env, ModulePropList ++ DefaultEnv}),
    IOList                                  = io_lib:format("~p.~n", [{application, AppName, AppData3}]),
    AppFile                                 = filename:join([OutDir, lists:concat([AppName, ".app"])]),
    file:write_file(AppFile, IOList).


make_computed_vsn({unknown, Val} ) ->Val;
make_computed_vsn(Cmd ) ->
    VsnString = os:cmd(Cmd),
    string:strip(VsnString, right, $\n).
        

reload_all() ->
    lager:notice("Reload All"),
    Modules = [M || {M, F} <- code:all_loaded(), is_list(F), not code:is_sticky(M)],
    [begin 
	 code:purge(M), 
	 code:load_file(M) 
     end || M <- Modules].

load_libraries(Application) ->
    load_libraries(Application, undefined).
load_libraries(Application, OutDir) ->
    load_dirs(boss_files_util:lib_path(), Application, OutDir, fun compile/2).

load_services_websockets(Application) ->
    load_services_websockets(Application, boss_files_util:ebin_dir()).
load_services_websockets(Application, OutDir) ->
    load_dirs(boss_files_util:websocket_path(), Application, OutDir, fun compile/2).

load_mail_controllers(Application) ->
    load_mail_controllers(Application, undefined).
load_mail_controllers(Application, OutDir) ->
    load_dirs(boss_files:mail_controller_path(), Application, OutDir, fun compile/2).

load_web_controllers(Application) ->
    load_web_controllers(Application, undefined).
load_web_controllers(Application, OutDir) ->
    load_dirs(boss_files_util:web_controller_path(), Application, OutDir, fun compile_controller/2).

load_view_lib_modules(Application) ->
    load_view_lib_modules(Application, undefined).
load_view_lib_modules(Application, OutDir) ->
    load_dirs(boss_files_util:view_helpers_path(), Application, OutDir, fun compile/2).

load_models(Application) ->
    load_models(Application, undefined).
load_models(Application, OutDir) ->
    load_model_dirs(boss_files_util:model_path(), Application, OutDir, fun compile_model/2).

load_model_dirs(Dirs, Application, OutDir, Compiler) ->
    load_model_dirs1(Dirs, Application, OutDir, Compiler, [], []).

load_model_dirs1([], _, _, _, ModuleAcc, []) ->
    {ok, ModuleAcc};
load_model_dirs1([], _, _, _, _, ErrorAcc) ->
    {error, ErrorAcc};
load_model_dirs1([Dir|Rest], Application, OutDir, Compiler, ModuleAcc, ErrorAcc) ->
    case load_model_dir(Dir, Application, OutDir, Compiler) of
        {ok, ModuleList} ->
            load_model_dirs1(Rest, Application, OutDir, Compiler, ModuleList ++ ModuleAcc, ErrorAcc);
        {error, ErrorList} ->
            load_model_dirs1(Rest, Application, OutDir, Compiler, ModuleAcc, ErrorList ++ ErrorAcc)
    end.

load_model_dir(Dir, Application, OutDir, Compiler) when is_function(Compiler) ->
    FullFiles = list_subfolder_files(Dir),    
    {ModuleList, ErrorList} = compile_and_accumulate_errors(
        FullFiles, Application, OutDir, Compiler, {[], []}),
    
    case length(ErrorList) of
        0 ->
            {ok, ModuleList};
        _ ->
            {error, ErrorList}
    end.

%% Only serve files that end in ".erl", with sub folder
%% todo: maybe remove  .#*.erl file (emacs specific)
list_subfolder_files(Dir) ->
    lists:filter(fun(String) ->
                         string:right(String, 4) == ".erl"
                 end, boss_files:find_file(Dir)).    


load_dirs(Dirs, Application, OutDir, Compiler) ->
    load_dirs1(Dirs, Application, OutDir, Compiler, [], []).

load_dirs1([], _, _, _, ModuleAcc, []) ->
    {ok, ModuleAcc};
load_dirs1([], _, _, _, _, ErrorAcc) ->
    {error, ErrorAcc};
load_dirs1([Dir|Rest], Application, OutDir, Compiler, ModuleAcc, ErrorAcc) ->
    case load_dir(Dir, Application, OutDir, Compiler) of
        {ok, ModuleList} ->
            load_dirs1(Rest, Application, OutDir, Compiler, ModuleList ++ ModuleAcc, ErrorAcc);
        {error, ErrorList} ->
            load_dirs1(Rest, Application, OutDir, Compiler, ModuleAcc, ErrorList ++ ErrorAcc)
    end.

load_dir(Dir, Application, OutDir, Compiler) when is_function(Compiler) ->
    Files     = list_files(Dir),
    FullFiles = lists:map(fun(F) -> filename:join([Dir, F]) end, Files),
    
    {ModuleList, ErrorList} = compile_and_accumulate_errors(
        FullFiles, Application, OutDir, Compiler, {[], []}),
    
    case length(ErrorList) of
        0 ->
            {ok, ModuleList};
        _ ->
            {error, ErrorList}
    end.

%% Only serve files that end in ".erl"
       
list_files(Dir) ->
    case file:list_dir(Dir) of
	{ok, FileList} ->
	   lists:filter(fun(String) ->
				string:right(String, 4) == ".erl"
			end, FileList);
	_ ->
	    []
    end.

compile_and_accumulate_errors([], _Application, _OutDir, _Compiler, Acc) -> 
    Acc;
compile_and_accumulate_errors([Filename|Rest], Application, OutDir, Compiler, {Modules, Errors}) ->
    Result = case filename:basename(Filename) of
        "."++_ ->
            {Modules, Errors};
        _ ->
            case filelib:is_dir(Filename) of
                true ->
                    case load_dir(Filename, Application, OutDir, Compiler) of
                        {ok, NewMods} ->
                            {NewMods ++ Modules, Errors};
                        {error, NewErrs} ->
                            {Modules, NewErrs ++ Errors}
                    end;
                false ->
                    CompileResult = maybe_compile(Filename, Application, OutDir, Compiler),
		    case CompileResult of
                        ok ->
                            {Modules, Errors};
                        {ok, Module} ->
                            {[Module|Modules], Errors};
                        {error, Error} ->
			    lager:error("Compile Error, ~p -> ~p", [Filename, Error]),
                            {Modules, [Error | Errors]};
                        {error, NewErrors, _NewWarnings} when is_list(NewErrors) ->
			    lager:error("Compile Error, ~p -> ~p", [Filename, NewErrors]),
                            {Modules, NewErrors ++ Errors}
                    end
            end
    end,
    compile_and_accumulate_errors(Rest, Application, OutDir, Compiler, Result).

maybe_compile(File, Application, OutDir, Compiler) ->
    CompilerAdapter = boss_files:compiler_adapter_for_extension(filename:extension(File)),
    maybe_compile(File, Application, OutDir, Compiler, CompilerAdapter).

maybe_compile(_File, _Application, _OutDir, _Compiler, undefined) -> ok;
maybe_compile(File, Application, OutDir, Compiler, CompilerAdapter) ->
    Module  = list_to_atom(CompilerAdapter:module_name_for_file(Application, File)),
    AbsPath = filename:absname(File),
    case OutDir of
	undefined ->
	    case module_older_than(Module, [AbsPath]) of
		true ->
		    Compiler(AbsPath, OutDir);
		_ ->
		    {ok, Module}
	    end;
	_ ->
	    Compiler(AbsPath, OutDir)
    end.

view_doc_root(ViewPath) ->
    lists:foldl(fun
            (LibPath, Best) when length(LibPath) > length(Best) ->
                case lists:prefix(LibPath, ViewPath) of
                    true ->
                        LibPath;
                    false ->
                        Best
                end;
            (_, Best) ->
                Best
        end, "",
        [boss_files_util:web_view_path(), boss_files_util:mail_view_path()]).

compile_view_dir_erlydtl(Application, LibPath, Module, OutDir, TranslatorPid) ->
    TagHelpers                = lists:map(fun erlang:list_to_atom/1, boss_files_util:view_tag_helper_list(Application)),
    FilterHelpers        = lists:map(fun erlang:list_to_atom/1, boss_files_util:view_filter_helper_list(Application)),
    ExtraTagHelpers	= boss_env:get_env(template_tag_modules, []),
    ExtraFilterHelpers	= boss_env:get_env(template_filter_modules, []),

    lager:info("Compile Modules ~p  ~p", [LibPath, Module]),
    Res = erlydtl:compile_dir(LibPath, Module,
                            [{doc_root, view_doc_root(LibPath)}, {compiler_options, []}, {out_dir, OutDir},
                             {custom_tags_modules, TagHelpers ++ ExtraTagHelpers ++ [boss_erlydtl_tags]},
                             {custom_filters_modules, FilterHelpers ++ ExtraFilterHelpers},
                             {blocktrans_fun,
                              fun(BlockString, Locale) ->
                                      case boss_translator:lookup(TranslatorPid, BlockString, Locale) of
                                          undefined -> default;
                                          Body -> list_to_binary(Body)
                                      end
                              end}]),
    case Res of
        ok ->
            {ok, Module};
        Err -> Err
    end.

compile_view(Application, ViewPath, TemplateAdapter, OutDir, TranslatorPid) ->
    case file:read_file_info(ViewPath) of
        {ok, _} ->
            Module		= view_module(Application, ViewPath),
            HelperDirModule	= view_custom_tags_dir_module(Application),
            Locales		= boss_files:language_list(Application),
            DocRoot		= view_doc_root(ViewPath),
            TagHelpers		= lists:map(fun erlang:list_to_atom/1, 
					    boss_files_util:view_tag_helper_list(Application)),
            FilterHelpers	= lists:map(fun erlang:list_to_atom/1, 
					    boss_files_util:view_filter_helper_list(Application)),
            TemplateAdapter:compile_file(ViewPath, Module, [
                    {out_dir, OutDir}, 
                    {doc_root, DocRoot},
                    {translator_pid, TranslatorPid},
                    {helper_module, HelperDirModule},
                    {tag_helpers, TagHelpers},
                    {filter_helpers, FilterHelpers},
                    {locales, Locales}]);
        _ ->
            {error, not_found}
    end.

compile_model(ModulePath, OutDir) ->
    IncludeDirs = [boss_files_util:include_dir() | boss_env:get_env(boss, include_dirs, [])],
    boss_model_manager:compile(ModulePath, [{out_dir, OutDir}, {include_dirs, IncludeDirs},
			 {compiler_options, compiler_options()}]).

compile_controller(ModulePath, OutDir) ->
    IncludeDirs = [boss_files_util:include_dir() | boss_env:get_env(boss, include_dirs, [])],
    Options = [{out_dir, OutDir}, {include_dirs, IncludeDirs}, {compiler_options, compiler_options()}],
    CompilerAdapter = boss_files:compiler_adapter_for_extension(filename:extension(ModulePath)),
    CompilerAdapter:compile_controller(ModulePath, Options).

compile(ModulePath, OutDir) ->
    IncludeDirs = [boss_files_util:include_dir() | boss_env:get_env(boss, include_dirs, [])],
    Options = [{out_dir, OutDir}, {include_dirs, IncludeDirs}, {compiler_options, compiler_options()}],
    CompilerAdapter = boss_files:compiler_adapter_for_extension(filename:extension(ModulePath)),
    CompilerAdapter:compile(ModulePath, Options).

compiler_options() ->
    lists:merge([{parse_transform, lager_transform}, return_errors], 
        boss_env:get_env(boss, compiler_options, [])).

load_view_lib(Application, OutDir, TranslatorPid) ->
    {ok, HelperDirModule} = compile_view_dir_erlydtl(Application,
        boss_files_util:view_html_tags_path(), view_custom_tags_dir_module(Application),
        OutDir, TranslatorPid),
    {ok, [HelperDirModule]}.

load_view_lib_if_old(Application, TranslatorPid) ->
    HelperDirModule = view_custom_tags_dir_module(Application),
    DirNeedsCompile = case module_is_loaded(HelperDirModule) of
        true ->
            module_older_than(HelperDirModule, lists:map(fun
                        ({File, _CheckSum}) -> File;
                        (File) -> File
                    end, [HelperDirModule:source_dir() | HelperDirModule:dependencies()]));
        false ->
            true
    end,
    case DirNeedsCompile of
        true ->
            load_view_lib(Application, undefined, TranslatorPid);
        false ->
            {ok, [HelperDirModule]}
    end.

load_views(Application, OutDir, TranslatorPid) ->
    ModuleList = lists:foldr(load_views_inner(Application, OutDir,
				              TranslatorPid),
			     [], boss_files:view_file_list()),
    {ok, ModuleList}.

load_views_inner(Application, OutDir, TranslatorPid) ->
    fun(File, Acc) ->
	    TemplateAdapter = boss_files:template_adapter_for_extension(
				filename:extension(File)),
	    ViewR = compile_view(Application, File, TemplateAdapter, OutDir, TranslatorPid),
	    case ViewR of
		{ok, Module} ->
		    [Module|Acc];
		{error, Reason} ->
		    lager:error("Unable to compile ~p because of ~p",
				[File, Reason]),
		    Acc
	    end
    end.

load_view_if_old(Application, ViewPath, Module, TemplateAdapter, TranslatorPid) ->
    case load_view_lib_if_old(Application, TranslatorPid) of
        {ok, _} -> 
            NeedCompile = case module_is_loaded(Module) of
                true ->
                    Dependencies = lists:map(fun
                            ({File, _CheckSum}) -> File;
                            (File) -> File
                        end, [TemplateAdapter:source(Module) | TemplateAdapter:dependencies(Module)]),
                    TagHelpers = lists:map(fun erlang:list_to_atom/1, boss_files_util:view_tag_helper_list(Application)),
                    FilterHelpers = lists:map(fun erlang:list_to_atom/1, boss_files_util:view_filter_helper_list(Application)),
                    ExtraTagHelpers = boss_env:get_env(template_tag_modules, []),
                    ExtraFilterHelpers = boss_env:get_env(template_filter_modules, []),
                    module_older_than(Module, 
                        Dependencies ++ TagHelpers ++ FilterHelpers ++ ExtraTagHelpers ++ ExtraFilterHelpers);
                false ->
                    true
            end,
            case NeedCompile of
                true ->
                    compile_view(Application, ViewPath, TemplateAdapter, 
                        undefined, TranslatorPid);
                false ->
                    {ok, Module}
            end
    end.

load_view_if_dev(Application, ViewPath, ViewModules, TranslatorPid) ->
    Module          = view_module(Application, ViewPath),
    TemplateAdapter = boss_files:template_adapter_for_extension(filename:extension(ViewPath)),
    case boss_env:is_developing_app(Application) of
        true -> 
            case load_view_if_old(Application, ViewPath, Module, TemplateAdapter, TranslatorPid) of
                {ok, Module} ->
                    {ok, Module, TemplateAdapter};
                Other ->
                    Other
            end;
        false -> 
            case lists:member(atom_to_list(Module), ViewModules) of
                true ->
                    {ok, Module, TemplateAdapter};
                _ ->
                    {error, not_found}
            end
    end.

module_is_loaded(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            true;
        _ ->
            false
    end.
-type maybe_list(X) :: X|list(X).
-spec(module_older_than(maybe_list(module()), maybe_list(string())) ->
              boolean()).
module_older_than(Module, Files) when is_atom(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            module_older_than(module_compiled_date(Module), Files);
        _ ->
            case code:load_file(Module) of
                {module, _} ->
                    case code:is_loaded(Module) of
                        {file, _} ->
                            module_older_than(module_compiled_date(Module), Files)
                    end;
                {error, _} ->
                    true
            end
    end;
module_older_than(Module, Files) when is_list(Module) ->
    module_older_than(filelib:last_modified(Module), Files);
module_older_than(_Date, []) ->
    false;
module_older_than(CompileDate, [File|Rest]) when is_list(File) ->
    module_older_than(CompileDate, [filelib:last_modified(File)|Rest]);
module_older_than(CompileDate, [Module|Rest]) when is_atom(Module) ->
    {file, Loaded} = code:is_loaded(Module),
    module_older_than(CompileDate, [Loaded|Rest]);
module_older_than(CompileDate, [CompareDate|Rest]) ->
    (CompareDate > CompileDate) orelse module_older_than(CompileDate, Rest).

module_compiled_date(Module) when is_atom(Module) ->
    try proplists:get_value(time, Module:module_info(compile)) of
        {Y,M,D,H,I,S} ->
            %% module compile times are in universal time, while
            %% file modification times are in localtime
            calendar:universal_time_to_local_time({{Y,M,D}, {H,I,S}});
        _ -> 0 %% 0 always less than any tuple
    catch
        _ -> 0
    end.

view_module(Application, RelativePath) ->
    Components   = tl(filename:split(RelativePath)),
    Lc           = string:to_lower(lists:concat([Application, "_", string:join(Components, "_")])),
    ModuleIOList = re:replace(Lc, "\\.", "_", [global]),
    list_to_atom(binary_to_list(iolist_to_binary(ModuleIOList))).

view_custom_tags_dir_module(Application) ->
    list_to_atom(lists:concat([Application, ?CUSTOM_TAGS_DIR_MODULE])).

incoming_mail_controller_module(Application) ->
    list_to_atom(lists:concat([Application, "_incoming_mail_controller"])).

vcs_vsn_cmd(git) ->
    case os:type() of
        {win32,nt} ->
            "FOR /F \"usebackq tokens=* delims=\" %i in "
            "(`git log -n 1 \"--pretty=format:%h\" .`) do "
            "@git describe --always --tags %i";
        _ ->
            "git describe --always --tags "
            "`git log -n 1 --pretty=format:%h .`"
    end;
vcs_vsn_cmd(hg)  -> "hg identify -i";
vcs_vsn_cmd(bzr) -> "bzr revno";
vcs_vsn_cmd(svn) -> "svnversion";
vcs_vsn_cmd("semver") ->
    lager:error("Use atom 'semver' not string \"semver\""),
    vcs_vsn_cmd(semver);
vcs_vsn_cmd(semver) ->
    case catch (rebar_vsn_plugin:make_vsn ()) of
        {'EXIT', _} -> {unknown, "semver"};
        Vsn         -> {unknown, Vsn}
    end;
vcs_vsn_cmd({cmd, _Cmd}=Custom) -> Custom;
vcs_vsn_cmd(Version) -> {unknown, Version}.

