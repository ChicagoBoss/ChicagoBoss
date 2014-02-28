%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
%% @author Dave Smith <dizzyd@dizzyd.com>
%% Adapted to boss compilation needs
%% @doc rebar_eunit supports the following commands:
%% <ul>
%%   <li>eunit - runs eunit tests</li>
%%   <li>clean - remove .eunit directory</li>
%%   <li>reset_after_eunit::boolean() - default = true.
%%       If true, try to "reset" VM state to approximate state prior to
%%       running the EUnit tests:
%%       <ul>
%%        <li>Stop net_kernel if it was started</li>
%%        <li>Stop OTP applications not running before EUnit tests were run</li>
%%        <li>Kill processes not running before EUnit tests were run</li>
%%        <li>Reset OTP application environment variables</li>
%%       </ul>
%%   </li>
%% </ul>
%% The following Global options are supported:
%% <ul>
%%   <li>verbose=1 - show extra output from the eunit test</li>
%%   <li>suite="foo"" - runs test/foo_tests.erl</li>
%% </ul>
%% Additionally, for projects that have separate folders for the core
%% implementation, and for the unit tests, then the following
%% <code>rebar.config</code> option can be provided:
%% <code>{eunit_compile_opts, [{src_dirs, ["dir"]}]}.</code>.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(boss_rebar_eunit).

-export([eunit/3,
         clean/2]).

-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(EUNIT_DIR, ".eunit").

%% ===================================================================
%% Public API
%% ===================================================================
eunit(RebarConf, BossConf, AppFile) ->
	%% boss_change
	Config = RebarConf,
    %% Make sure ?EUNIT_DIR/ and ebin/ directory exists (tack on dummy module)
    ok = filelib:ensure_dir(eunit_dir() ++ "/foo"),
    ok = filelib:ensure_dir(ebin_dir() ++ "/foo"),

    %% Setup code path prior to compilation so that parse_transforms
    %% and the like work properly. Also, be sure to add ebin_dir()
    %% to the END of the code path so that we don't have to jump
    %% through hoops to access the .app file
    CodePath = code:get_path(),

    true = code:add_patha(eunit_dir()),
    true = code:add_pathz(ebin_dir()),

    %% Obtain all the test modules for inclusion in the compile stage.
	%% boss_change: compile only src/test/eunit tests
    TestErls = rebar_utils:find_files(filename:join(["src", "test", "eunit"]), ".*\\.erl\$"),

    %% Copy source files to eunit dir for cover in case they are not directly
    %% in src but in a subdirectory of src. Cover only looks in cwd and ../src
    %% for source files.
    SrcErls = rebar_utils:find_files("src", ".*\\.erl\$"),

    %% If it is not the first time rebar eunit is executed, there will be source
    %% files already present in ?EUNIT_DIR. Since some SCMs (like Perforce) set
    %% the source files as being read only (unless they are checked out), we
    %% need to be sure that the files already present in ?EUNIT_DIR are writable
    %% before doing the copy. This is done here by removing any file that was
    %% already present before calling rebar_file_utils:cp_r.

    %% Get the full path to a file that was previously copied in ?EUNIT_DIR
    ToCleanUp = fun(F, Acc) ->
                        F2 = filename:basename(F),
                        F3 = filename:join([?EUNIT_DIR, F2]),
                        case filelib:is_regular(F3) of
                            true -> [F3|Acc];
                            false -> Acc
                        end
                end,

    ok = rebar_file_utils:delete_each(lists:foldl(ToCleanUp, [], TestErls)),
    ok = rebar_file_utils:delete_each(lists:foldl(ToCleanUp, [], SrcErls)),
	
    %% boss_change
    %ok = rebar_file_utils:cp_r(SrcErls ++ TestErls, ?EUNIT_DIR),

    %% boss_change
    %% Compilation (boss-way)
    boss_rebar:compile(RebarConf, BossConf, AppFile, ?EUNIT_DIR),    

    %% Compile erlang code to ?EUNIT_DIR, using a tweaked config
    %% with appropriate defines for eunit, and include all the test modules
    %% as well.

	rebar_erlc_compiler:doterl_compile(eunit_config(Config),
                                       ?EUNIT_DIR, TestErls),
	%% boss_change
	%% Load all boss ebin dir and start boss
	boss_rebar:boss_load(BossConf, AppFile),
	boss_rebar:boss_start(BossConf),

    %% Build a list of all the .beams in ?EUNIT_DIR -- use this for
    %% cover and eunit testing. Normally you can just tell cover
    %% and/or eunit to scan the directory for you, but eunit does a
    %% code:purge in conjunction with that scan and causes any cover
    %% compilation info to be lost.  Filter out "*_tests" modules so
    %% eunit won't doubly run them and so cover only calculates
    %% coverage on production code.  However, keep "*_tests" modules
    %% that are not automatically included by eunit.
    AllBeamFiles = rebar_utils:beams(?EUNIT_DIR),
    {BeamFiles, TestBeamFiles} =
        lists:partition(fun(N) -> string:str(N, "_tests.beam") =:= 0 end,
                        AllBeamFiles),
    OtherBeamFiles = TestBeamFiles --
        [filename:rootname(N) ++ "_tests.beam" || N <- AllBeamFiles],
    ModuleBeamFiles = BeamFiles ++ OtherBeamFiles,
    Modules = [rebar_utils:beam_to_mod(?EUNIT_DIR, N) || N <- ModuleBeamFiles],
    SrcModules = [rebar_utils:erl_to_mod(M) || M <- SrcErls],

    {ok, CoverLog} = cover_init(Config, ModuleBeamFiles),

    StatusBefore = status_before_eunit(),

    EunitResult = perform_eunit(Config, Modules),
    perform_cover(Config, Modules, SrcModules),

    cover_close(CoverLog),

    case proplists:get_value(reset_after_eunit, get_eunit_opts(Config),
                             true) of
        true ->
            reset_after_eunit(StatusBefore);
        false ->
            ok
    end,

    case EunitResult of
        ok ->
            ok;
        _ ->
            ?ABORT("One or more eunit tests failed.~n", [])
    end,

    %% Restore code path
    true = code:set_path(CodePath),
    ok.

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?EUNIT_DIR).

%% ===================================================================
%% Internal functions
%% ===================================================================

eunit_dir() ->
    filename:join(rebar_utils:get_cwd(), ?EUNIT_DIR).

ebin_dir() ->
    filename:join(rebar_utils:get_cwd(), "ebin").

perform_eunit(Config, Modules) ->
    %% suite defined, so only specify the module that relates to the
    %% suite (if any). Suite can be a comma seperated list of modules to run.
    Suite = rebar_config:get_global(Config, suite, undefined),
    EunitOpts = get_eunit_opts(Config),

    %% Move down into ?EUNIT_DIR while we run tests so any generated files
    %% are created there (versus in the source dir)
    %%Cwd = rebar_utils:get_cwd(),
    %%ok = file:set_cwd(?EUNIT_DIR),

    EunitResult = perform_eunit(EunitOpts, Modules, Suite),

    %% Return to original working dir
    %%ok = file:set_cwd(Cwd),

    EunitResult.

perform_eunit(EunitOpts, Modules, undefined) ->
    (catch eunit:test(Modules, EunitOpts));
perform_eunit(EunitOpts, _Modules, Suites) ->
    (catch eunit:test([list_to_atom(Suite) ||
                          Suite <- string:tokens(Suites, ",")], EunitOpts)).

get_eunit_opts(Config) ->
    %% Enable verbose in eunit if so requested..
    BaseOpts = case rebar_log:is_verbose(Config) of
                   true ->
                       [verbose];
                   false ->
                       []
               end,

    BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []).

eunit_config(Config) ->
    EqcOpts = eqc_opts(),
    PropErOpts = proper_opts(),

    ErlOpts = rebar_config:get_list(Config, erl_opts, []),
    EunitOpts = rebar_config:get_list(Config, eunit_compile_opts, []),
    Opts0 = [{d, 'TEST'}] ++
        ErlOpts ++ EunitOpts ++ EqcOpts ++ PropErOpts,
    Opts = [O || O <- Opts0, O =/= no_debug_info],
    Config1 = rebar_config:set(Config, erl_opts, Opts),

    FirstErls = rebar_config:get_list(Config1, eunit_first_files, []),
    rebar_config:set(Config1, erl_first_files, FirstErls).

eqc_opts() ->
    define_if('EQC', is_lib_avail(is_eqc_avail, eqc,
                                  "eqc.hrl", "QuickCheck")).

proper_opts() ->
    define_if('PROPER', is_lib_avail(is_proper_avail, proper,
                                     "proper.hrl", "PropEr")).

define_if(Def, true) -> [{d, Def}];
define_if(_Def, false) -> [].

is_lib_avail(DictKey, Mod, Hrl, Name) ->
    case erlang:get(DictKey) of
        undefined ->
            IsAvail = case code:lib_dir(Mod, include) of
                          {error, bad_name} ->
                              false;
                          Dir ->
                              filelib:is_regular(filename:join(Dir, Hrl))
                      end,
            erlang:put(DictKey, IsAvail),
            ?DEBUG("~s availability: ~p\n", [Name, IsAvail]),
            IsAvail;
        IsAvail ->
            IsAvail
    end.

perform_cover(Config, BeamFiles, SrcModules) ->
    perform_cover(rebar_config:get(Config, cover_enabled, false),
                  Config, BeamFiles, SrcModules).

perform_cover(false, _Config, _BeamFiles, _SrcModules) ->
    ok;
perform_cover(true, Config, BeamFiles, SrcModules) ->
    cover_analyze(Config, BeamFiles, SrcModules).

cover_analyze(_Config, [], _SrcModules) ->
    ok;
cover_analyze(Config, Modules, SrcModules) ->
    %% suite can be a comma seperated list of modules to test
    Suite = [list_to_atom(S) ||
                S <- string:tokens(rebar_config:get_global(Config, suite, ""), ",")],
    FilteredModules = case Suite of
                          [] -> Modules;
                          _  -> [M || M <- Modules, lists:member(M, Suite)]
                      end,

    %% Generate coverage info for all the cover-compiled modules
    Coverage = lists:flatten([cover_analyze_mod(M) || M <- FilteredModules]),

    %% Write index of coverage info
    cover_write_index(lists:sort(Coverage), SrcModules),

    %% Write coverage details for each file
    lists:foreach(fun({M, _, _}) ->
                          {ok, _} = cover:analyze_to_file(M, cover_file(M),
                                                          [html])
                  end, Coverage),

    Index = filename:join([rebar_utils:get_cwd(), ?EUNIT_DIR, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]),

    %% Print coverage report, if configured
    case rebar_config:get(Config, cover_print_enabled, false) of
        true ->
            cover_print_coverage(lists:sort(Coverage));
        false ->
            ok
    end.

cover_close(not_enabled) ->
    ok;
cover_close(F) ->
    ok = file:close(F).

cover_init(false, _BeamFiles) ->
    {ok, not_enabled};
cover_init(true, BeamFiles) ->
    %% Attempt to start the cover server, then set it's group leader to
    %% .eunit/cover.log, so all cover log messages will go there instead of
    %% to stdout. If the cover server is already started we'll reuse that
    %% pid.
    {ok, CoverPid} = case cover:start() of
                         {ok, _P} = OkStart ->
                             OkStart;
                         {error,{already_started, P}} ->
                             {ok, P};
                         {error, _Reason} = ErrorStart ->
                             ErrorStart
                     end,

    {ok, F} = OkOpen = file:open(
                         filename:join([?EUNIT_DIR, "cover.log"]),
                         [write]),

    group_leader(F, CoverPid),

    %% Make sure any previous runs of cover don't unduly influence
    cover:reset(),

    ?INFO("Cover compiling ~s\n", [rebar_utils:get_cwd()]),

    Compiled = [{Beam, cover:compile_beam(Beam)} || Beam <- BeamFiles],
    case [Module || {_, {ok, Module}} <- Compiled] of
        [] ->
            %% No modules compiled successfully...fail
            ?ERROR("Cover failed to compile any modules; aborting.~n", []),
            ?FAIL;
        _ ->
            %% At least one module compiled successfully

            %% It's not an error for cover compilation to fail partially,
            %% but we do want to warn about them
            PrintWarning =
                fun(Beam, Desc) ->
                        ?CONSOLE("Cover compilation warning for ~p: ~p",
                                 [Beam, Desc])
                end,
            _ = [PrintWarning(Beam, Desc) || {Beam, {error, Desc}} <- Compiled],
            OkOpen
    end;
cover_init(Config, BeamFiles) ->
    cover_init(rebar_config:get(Config, cover_enabled, false), BeamFiles).

cover_analyze_mod(Module) ->
    case cover:analyze(Module, coverage, module) of
        {ok, {Module, {Covered, NotCovered}}} ->
            %% Modules that include the eunit header get an implicit
            %% test/0 fun, which cover considers a runnable line, but
            %% eunit:test(TestRepresentation) never calls.  Decrement
            %% NotCovered in this case.
            [align_notcovered_count(Module, Covered, NotCovered,
                                    is_eunitized(Module))];
        {error, Reason} ->
            ?ERROR("Cover analyze failed for ~p: ~p ~p\n",
                   [Module, Reason, code:which(Module)]),
            []
    end.

is_eunitized(Mod) ->
    has_eunit_test_fun(Mod) andalso
        has_header(Mod, "include/eunit.hrl").

has_eunit_test_fun(Mod) ->
    [F || {exports, Funs} <- Mod:module_info(),
          {F, 0} <- Funs, F =:= test] =/= [].

has_header(Mod, Header) ->
    Mod1 = case code:which(Mod) of
               cover_compiled ->
                   {file, File} = cover:is_compiled(Mod),
                   File;
               non_existing -> Mod;
               preloaded -> Mod;
               L -> L
           end,
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Mod1,
                                                            [abstract_code]),
    [F || {attribute, 1, file, {F, 1}} <- AC,
          string:str(F, Header) =/= 0] =/= [].

align_notcovered_count(Module, Covered, NotCovered, false) ->
    {Module, Covered, NotCovered};
align_notcovered_count(Module, Covered, NotCovered, true) ->
    {Module, Covered, NotCovered - 1}.

cover_write_index(Coverage, SrcModules) ->
    {ok, F} = file:open(filename:join([?EUNIT_DIR, "index.html"]), [write]),
    ok = file:write(F, "<html><head><title>Coverage Summary</title></head>\n"),
    IsSrcCoverage = fun({Mod,_C,_N}) -> lists:member(Mod, SrcModules) end,
    {SrcCoverage, TestCoverage} = lists:partition(IsSrcCoverage, Coverage),
    cover_write_index_section(F, "Source", SrcCoverage),
    cover_write_index_section(F, "Test", TestCoverage),
    ok = file:write(F, "</body></html>"),
    ok = file:close(F).

cover_write_index_section(_F, _SectionName, []) ->
    ok;
cover_write_index_section(F, SectionName, Coverage) ->
    %% Calculate total coverage %
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Write the report
    ok = file:write(F, ?FMT("<body><h1>~s Summary</h1>\n", [SectionName])),
    ok = file:write(F, ?FMT("<h3>Total: ~s</h3>\n", [TotalCoverage])),
    ok = file:write(F, "<table><tr><th>Module</th><th>Coverage %</th></tr>\n"),

    FmtLink =
        fun(Module, Cov, NotCov) ->
                ?FMT("<tr><td><a href='~s.COVER.html'>~s</a></td><td>~s</td>\n",
                     [Module, Module, percentage(Cov, NotCov)])
        end,
    lists:foreach(fun({Module, Cov, NotCov}) ->
                          ok = file:write(F, FmtLink(Module, Cov, NotCov))
                  end, Coverage),
    ok = file:write(F, "</table>\n").

cover_print_coverage(Coverage) ->
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Determine the longest module name for right-padding
    Width = lists:foldl(fun({Mod, _, _}, Acc) ->
                                case length(atom_to_list(Mod)) of
                                    N when N > Acc ->
                                        N;
                                    _ ->
                                        Acc
                                end
                        end, 0, Coverage) * -1,

    %% Print the output the console
    ?CONSOLE("~nCode Coverage:~n", []),
    lists:foreach(fun({Mod, C, N}) ->
                          ?CONSOLE("~*s : ~3s~n",
                                   [Width, Mod, percentage(C, N)])
                  end, Coverage),
    ?CONSOLE("~n~*s : ~s~n", [Width, "Total", TotalCoverage]).

cover_file(Module) ->
    filename:join([?EUNIT_DIR, atom_to_list(Module) ++ ".COVER.html"]).

percentage(0, 0) ->
    "not executed";
percentage(Cov, NotCov) ->
    integer_to_list(trunc((Cov / (Cov + NotCov)) * 100)) ++ "%".

get_app_names() ->
    [AppName || {AppName, _, _} <- application:loaded_applications()].

status_before_eunit() ->
    Apps = get_app_names(),
    AppEnvs = [{App, application:get_all_env(App)} || App <- Apps],
    {erlang:processes(), erlang:is_alive(), AppEnvs, ets:tab2list(ac_tab)}.

reset_after_eunit({OldProcesses, WasAlive, OldAppEnvs, _OldACs}) ->
    IsAlive = erlang:is_alive(),
    if not WasAlive andalso IsAlive ->
            ?DEBUG("Stopping net kernel....\n", []),
            erl_epmd:stop(),
            _ = net_kernel:stop(),
            pause_until_net_kernel_stopped();
       true ->
            ok
    end,

    OldApps = [App || {App, _} <- OldAppEnvs],
    Apps = get_app_names(),
    _ = [begin
             _ = case lists:member(App, OldApps) of
                     true  -> ok;
                     false -> application:stop(App)
                 end,
             ok = application:unset_env(App, K)
         end || App <- Apps, App /= rebar,
                {K, _V} <- application:get_all_env(App)],

    reconstruct_app_env_vars(Apps),

    Processes = erlang:processes(),
    _ = kill_extras(Processes -- OldProcesses),

    ok.

kill_extras(Pids) ->
    %% Killing any of the procs below will either:
    %% 1. Interfere with stuff that we don't want interfered with, or
    %% 2. May/will force the 'kernel' app to shutdown, which *will*
    %%    interfere with rebar's ability To Do Useful Stuff(tm).
    %% This list may require changes as OTP versions and/or
    %% rebar use cases change.
    KeepProcs = [cover_server, eunit_server,
                 eqc, eqc_license, eqc_locked,
                 %% inet_gethost_native is started on demand, when
                 %% doing name lookups. It is under kernel_sup, under
                 %% a supervisor_bridge.
                 inet_gethost_native],
    Killed = [begin
                  Info = case erlang:process_info(Pid) of
                             undefined -> [];
                             Else      -> Else
                         end,
                  Keep1 = case proplists:get_value(registered_name, Info) of
                              undefined ->
                                  false;
                              Name ->
                                  lists:member(Name, KeepProcs)
                          end,
                  Keep2 = case proplists:get_value(dictionary, Info) of
                              undefined ->
                                  false;
                              Ds ->
                                  case proplists:get_value('$ancestors', Ds) of
                                      undefined ->
                                          false;
                                      As ->
                                          lists:member(kernel_sup, As)
                                  end
                          end,
                  if Keep1 orelse Keep2 ->
                          ok;
                     true ->
                          ?DEBUG("Kill ~p ~p\n", [Pid, Info]),
                          exit(Pid, kill),
                          Pid
                  end
              end || Pid <- Pids],
    case lists:usort(Killed) -- [ok] of
        [] ->
            ?DEBUG("No processes to kill\n", []),
            [];
        Else ->
            lists:foreach(fun(Pid) -> wait_until_dead(Pid) end, Else),
            Else
    end.

reconstruct_app_env_vars([App|Apps]) ->
    CmdLine0 = proplists:get_value(App, init:get_arguments(), []),
    CmdVars = [{list_to_atom(K), list_to_atom(V)} || {K, V} <- CmdLine0],
    AppFile = (catch filename:join([code:lib_dir(App),
                                    "ebin",
                                    atom_to_list(App) ++ ".app"])),
    AppVars = case file:consult(AppFile) of
                  {ok, [{application, App, Ps}]} ->
                      proplists:get_value(env, Ps, []);
                  _ ->
                      []
              end,
    AllVars = CmdVars ++ AppVars,
    ?DEBUG("Reconstruct ~p ~p\n", [App, AllVars]),
    lists:foreach(fun({K, V}) -> application:set_env(App, K, V) end, AllVars),
    reconstruct_app_env_vars(Apps);
reconstruct_app_env_vars([]) ->
    ok.

wait_until_dead(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _Obj, Info} ->
            Info
    after 10*1000 ->
            exit({timeout_waiting_for, Pid})
    end;
wait_until_dead(_) ->
    ok.

pause_until_net_kernel_stopped() ->
    pause_until_net_kernel_stopped(10).

pause_until_net_kernel_stopped(0) ->
    exit(net_kernel_stop_failed);
pause_until_net_kernel_stopped(N) ->
    try
        _ = net_kernel:i(),
        timer:sleep(100),
        pause_until_net_kernel_stopped(N - 1)
    catch
        error:badarg ->
            ?DEBUG("Stopped net kernel.\n", []),
            ok
    end.
