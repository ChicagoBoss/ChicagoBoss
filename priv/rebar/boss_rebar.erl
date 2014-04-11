%%%-------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% @doc Chicago Boss rebar functions, called from boss_plugin
%%%  Managing compilation/configuration/scripts stuff, the boss way
%%% @end
%%%-------------------------------------------------------------------
-module(boss_rebar).

-export([run/4,
         run/5, 
         help/0, 
         help/3,
         compile/3,
         compile/4,
         test_eunit/3,
         test_functional/3,
         start_cmd/3,
         start_dev_cmd/3,
         stop_cmd/3,
         reload_cmd/3,
         attach_cmd/3,
         boss_config_value/3,
         boss_config_value/4,
         boss_load/2,
         boss_start/1,
         all_ebin_dirs/2,
         init_conf/1
        ]).

-define(COMMANDS, 
		[{help,  "Lists all commands"},
		 {compile, "Compile (boss way)"},
		 {test_eunit, "Run src/test/eunit tests"},
		 {test_functional, "Run src/test/functional tests"},
		 {start_cmd, "Generates the start shell command"},
		 {start_dev_cmd, "Generates the start-dev shell command"},
		 {stop_cmd, "Generates the stop shell command"},
		 {reload_cmd, "Generates the hot reload shell command"},
                 {attach_cmd, "Attach the production system console"}
		]).

-define(BOSS_PLUGIN_VERSION, 1).
-define(ERLANG_MIN_VERSION, "15").

%%--------------------------------------------------------------------
%% @doc run
%% @spec run(Command, RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Rebar Commands Router
%% @end
%%--------------------------------------------------------------------
run(_,_,BossConf,_) ->
    report_bad_client_version_and_exit(BossConf).
run(Version, Command, RebarConf, BossConf, AppFile) when is_list(Command)->
	run(Version, list_to_atom(Command), RebarConf, BossConf, AppFile);
run(Version, Command, RebarConf, BossConf, AppFile) ->
    rebar_log:log(debug, "Checking rebar plugin client version and Erlang runtime version'~s'~n", [Command]),
    ErlVsn = otp_version(),
    case Version =:= ?BOSS_PLUGIN_VERSION of
        false ->
            report_bad_client_version_and_exit(BossConf);
        true when ErlVsn < ?ERLANG_MIN_VERSION ->
	    report_old_erlang_version_and_exit(ErlVsn);
        true ->
            rebar_log:log(debug, "About to run command '~s'~n", [Command]),
        	case lists:keyfind(Command, 1, ?COMMANDS) of
        		false -> 
        			{error, command_not_found};
        		_ -> 
        			apply(boss_rebar, Command, [RebarConf, BossConf, AppFile])
        	end
    end.

otp_version() ->
    case erlang:system_info(otp_release) of
        "R" ++ Version -> Version;
        Version -> Version
    end.

%%--------------------------------------------------------------------
%% @doc compile
%% @spec compile(RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Chicago Boss compilation, handling all cb special needs 
%% @end
%%--------------------------------------------------------------------
compile(RebarConf, BossConf, AppFile) ->
    compile(RebarConf, BossConf, AppFile, "ebin").
%%--------------------------------------------------------------------
%% @doc compile
%% @spec compile(_RebarConf, _BossConf, AppFile, Dest) -> 
%%                                                ok | {error, Reason}
%%       Chicago Boss compilation, handling all cb special needs 
%% @end
%%--------------------------------------------------------------------
compile(RebarConf, BossConf, AppFile, Dest) ->
    boss_load(BossConf, AppFile),
    %% Everything in Chicago Boss needs lager, so we need to make sure it starts.
    ok = lager:start(),

    %% We also set the log level for lager according to what we have in Rebar.
    Level = rebar_config:get_global(RebarConf, verbose, rebar_log:default_level()),
    NewLevel = case Level of
                   0 -> error;
                   1 -> warning;
                   2 -> info;
                   3 -> debug
               end,

    lager:set_loglevel(lager_console_backend, NewLevel),

    AppName = app_name(AppFile),
    Res = boss_load:load_all_modules_and_emit_app_file(AppName, Dest),
    rebar_log:log(info, "Chicago Boss compilation of app ~s on ~s (~s)~n", 
                  [AppName, Dest, Res]).

%%--------------------------------------------------------------------
%% @doc test_eunit
%% @spec test_eunit(_RebarConf, _BossConf, AppFile) -> 
%%                                                ok | {error, Reason}
%%       Chicago Boss eunit integration 
%% @end
%%--------------------------------------------------------------------
test_eunit(RebarConf, BossConf, AppFile) ->
    boss_rebar_eunit:eunit(RebarConf, BossConf, AppFile).

%%--------------------------------------------------------------------
%% @doc test_functional
%% @spec test_functional(_RebarConf, _BossConf, AppFile) -> 
%%                                                ok | {error, Reason}
%%       Chicago Boss functionals test
%%       API: http://www.chicagoboss.org/api-test.html
%% @end
%%--------------------------------------------------------------------
test_functional(RebarConf, BossConf, AppFile) ->
	%% Compile, load all boss ebin dir and start boss
	boss_rebar:compile(RebarConf, BossConf, AppFile),
	boss_rebar:boss_load(BossConf, AppFile),
	% boss_rebar:boss_start(BossConf),
	AppName = app_name(AppFile),
	boss_web_test:start([atom_to_list(AppName)]).

%%--------------------------------------------------------------------
%% @doc start_cmd
%% @spec start_cmd(RebarConf, BossConf, AppFile) -> 
%%                                                ok | {error, Reason}
%%       Generate start shell command (production)
%% @end
%%--------------------------------------------------------------------
start_cmd(_RebarConf, BossConf, AppFile) ->
  rebar_log:log(info, "Generating dynamic start command~n", []),

  EbinDirs = all_ebin_dirs(BossConf, AppFile),
  MaxProcesses = max_processes(BossConf),
  NameArg = vm_name_arg(BossConf, AppFile),
  CookieOpt = cookie_option(BossConf),

  ErlCmd = erl_command(),
  VmArgs = vm_args(BossConf),
  io:format("~s +K true +P ~B -pa ~s -boot start_sasl -config boss -s boss ~s -detached ~s~s~n",
    [ErlCmd, MaxProcesses, string:join(EbinDirs, " -pa "), CookieOpt, NameArg, VmArgs]),
  ok.

%%--------------------------------------------------------------------
%% @doc start_dev_cmd
%% @spec start_dev_cmd(RebarConf, BossConf, AppFile) -> 
%%                                                ok | {error, Reason}
%%       Generate start-dev shell command (development)
%% @end
%%--------------------------------------------------------------------
start_dev_cmd(_RebarConf, BossConf, AppFile) ->
  rebar_log:log(info, "Generating dynamic start-dev command~n", []),

  AppName	= app_name(AppFile),
  NameArg	= vm_name_arg(BossConf, AppFile),
  ErlCmd	= erl_command(),
  EbinDirs	= all_ebin_dirs(BossConf, AppFile),
  CookieOpt	= cookie_option(BossConf),
  VmArgs	= vm_args(BossConf),
  io:format("~s -pa ~s -boss developing_app ~s -boot start_sasl -config boss ~s -s reloader -s lager -s boss ~s~s~n",
    [ErlCmd, string:join(EbinDirs, " -pa "), AppName, CookieOpt, NameArg, VmArgs]),
  ok.

%%--------------------------------------------------------------------
%% @doc stop_cmd
%% @spec stop_cmd(RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Generate stop shell command (production)
%% @end
%%--------------------------------------------------------------------
stop_cmd(_RebarConf, BossConf, AppFile) ->
  rebar_log:log(info, "Generating dynamic stop command~n", []),

  NameArg = vm_name_arg(BossConf, AppFile, "stopper_"),

  NameQualif = wm_qualified_name(vm_name(BossConf, AppFile)),

  CookieOpt = cookie_option(BossConf),
  StopCommand = io_lib:format("rpc:call('~s', init, stop, []).", [NameQualif]),
  io:format("~s -noshell -pa ebin ~s ~s -eval \"~s\" -s init stop",
    [erl_command(), CookieOpt, NameArg, StopCommand]),
  ok.

%%--------------------------------------------------------------------
%% @doc reload_cmd
%% @spec reload_cmd(RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Generate hot reload shell command (production)
%% @end
%%--------------------------------------------------------------------
reload_cmd(_RebarConf, BossConf, AppFile) ->
  rebar_log:log(info, "Generating dynamic reload command~n", []),
  NameArg = vm_name_arg(BossConf, AppFile, "reloader_"),

  NameQualif = wm_qualified_name(vm_name(BossConf, AppFile)),

  CookieOpt = cookie_option(BossConf),
  ReloadCode = io_lib:format("rpc:call('~s', boss_load, reload_all, [])", [NameQualif]),
  ReloadRoutes = io_lib:format("rpc:call('~s', boss_web, reload_routes, [])", [NameQualif]),
  ReloadLangs = io_lib:format("rpc:call('~s', boss_web, reload_all_translations, [])", [NameQualif]),
  io:format("~s -noshell -pa ebin ~s ~s -eval \"~s, ~s, ~s.\" -s init stop",
    [erl_command(), CookieOpt, NameArg, ReloadCode, ReloadRoutes, ReloadLangs]),
  ok.
%%--------------------------------------------------------------------
%% @doc attach_cmd
%% @spec attach_cmd(RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Attach to the production system console
%% @end
%%--------------------------------------------------------------------
attach_cmd(_RebarConf, BossConf, AppFile) ->
  rebar_log:log(info, "Generating dynamic attach command~n", []),
  Prefix = io_lib:format("console_~s_", [os:getpid()]),
  NameArg = vm_name_arg(BossConf, AppFile, Prefix),
  NameQualif = wm_qualified_name(vm_name(BossConf, AppFile)),

  CookieOpt = cookie_option(BossConf),
  io:format("~s ~s -remsh ~s ~s",
    [erl_command(), CookieOpt, NameQualif, NameArg]),
  ok.

%%--------------------------------------------------------------------
%% @doc help print known commands
%% @spec help(_RebarConf, _BossConf, _AppFile) -> Help
%%       print known commands
%% @end
%%--------------------------------------------------------------------
help() -> help(true, true, true).
help(_RebarConf, _BossConf, _AppFile) ->
    S = <<"
# Chicago Boss rebar commands
# ----------------------------------------------------------------------------
#
# For init commands (autogenerated) from boss.config, run:
# ./init.sh

# Compilation
compile                              Compile sources (compat.)
boss c=compile                       Compile sources (called from compile)

# Tests
eunit                   [suite=foo]  Run src/test/eunit tests (compat.)
boss c=test_eunit       [suite=foo]  Run src/test/eunit tests
boss c=test_functional               Run src/test/functional tests

# Help
boss                                 Show this help
boss c=help                          Show this help
">>,
    io:put_chars(S).

%%--------------------------------------------------------------------
%% @doc Get Boss config value app, key, default
%% @spec boss_config_value(BossConfig, App, Key, Default) -> 
%%                                         Value | Default
%%       Searchs in boss config for a given App and Key,
%%       Don't return an error, return Default
%% @end
%%--------------------------------------------------------------------
boss_config_value(BossConf, App, Key, Default) ->
	case boss_config_value(BossConf, App, Key) of
		{error, _} -> Default;
		Value -> Value
	end.
  
%%--------------------------------------------------------------------
%% @doc Get Boss config value app, key
%% @spec boss_config_value(BossConfig, App, Key) -> 
%%                                         Value | {error, Reason}
%%       Searchs in boss config for a given App and Key
%% @end
%%--------------------------------------------------------------------
boss_config_value(BossConf, App, Key) ->
	case lists:keyfind(App, 1, BossConf) of
		false -> 
			{error, boss_config_app_not_found};
		{App, AppConf} -> 
			case lists:keyfind(Key, 1, AppConf) of
				false -> 
					{error, boss_config_app_setting_not_found};
				{Key, KeyConf} ->
					KeyConf
			end
	end.

%%--------------------------------------------------------------------
%% @doc Load all boss and app beam code
%% @spec boss_load(BossConfig)
%% @end
%%--------------------------------------------------------------------
boss_load(BossConf, AppFile) ->
	%% Get all path-defined apps from boss config
	%% Exclude current boss app if allready loaded
	AppCurrent = app_name(AppFile),
	
	AllDirs = lists:foldl(fun({App, Config}, Dirs) ->
		case {lists:keyfind(path, 1, Config), AppCurrent =:= App}  of
            {false, false} ->
                [filename:join(["deps", atom_to_list(App)])|Dirs];
            {false, _} -> 
				Dirs;
			{{path, Path}, true} ->
				case filelib:is_regular(filename:join(["ebin", atom_to_list(App) ++ ".app"])) of
					true -> Dirs;
					false -> [Path|Dirs]
				end;
			{{path, Path}, _} -> [Path|Dirs]
		end end, [], lists:reverse(BossConf)),

    lists:map(fun(Dir) ->
                      lists:map(fun(B) ->
                                        F = string:substr(B, 1, string:len(B) - string:len(".beam")),
                                        case code:is_loaded(list_to_atom(filename:basename(F))) of
                                            false ->
                                                code:load_abs(F);
                                            {file, _} ->
                                                ok
                                        end 
                                 end, rebar_utils:beams(Dir))
              end, AllDirs),
    %% Fix starting mimetypes app in boss.erl->ensure_started(mimetypes)
    %% mimetyps.app not found, adding deps/*/ebin don't work
    BossPath = get_boss_path(BossConf),
    code:add_path(BossPath++"/deps/mimetypes/ebin"),
    code:add_path(BossPath++"/deps/goldrush/ebin"),
    code:add_path(BossPath++"/deps/lager/ebin").

%%--------------------------------------------------------------------
%% @doc Start the boss app
%% @spec boss_start(BossConfig)
%% @end
%%--------------------------------------------------------------------
boss_start(_BossConf) ->
    io:format("Starting boss and waiting for all apps to initialize...~n"),
    ok = boss:start(),
    BossApps = boss_env:get_env(applications, []),
    timer:sleep(50),
    boss_start_wait(BossApps),
    rebar_log:log(info, "All Boss Apps started~n", []),
    ok.

%%--------------------------------------------------------------------
%% @doc Checks that all boss apps (not core) is started
%% @spec boss_start_wait(BossApps::list())
%% @end
%%--------------------------------------------------------------------
boss_start_wait([]) -> ok;
boss_start_wait([App|Rest]) ->
    CurrentApps = application:which_applications(),
    case lists:keyfind(App, 1, CurrentApps) of
        false ->
            rebar_log:log(info, "Boss App ~p still not started, waiting...~n", [App]),
            timer:sleep(50),
            boss_start_wait([App|Rest]);
        _ ->
            rebar_log:log(info, "Boss App ~p started~n", [App]),
            boss_start_wait(Rest)
    end.

%%--------------------------------------------------------------------
%% @doc Start the boss app
%% @spec all_ebin_dirs(BossConf, _AppFile)
%%       Gets all ebin dirs for the apps defined in boss.config
%% @end
%%--------------------------------------------------------------------
all_ebin_dirs(BossConf, AppFile) ->
    BossAppEbinDir = all_boss_app_ebin_dirs(BossConf, AppFile) ++ all_deps_ebin_dirs(AppFile),
    MoreEbins = lists:foldl(fun({App, Config}, EbinDirs) ->
                        case lists:keyfind(path, 1, Config) of
                            false -> EbinDirs;
                            {path, Path} ->
                                case lists:reverse(filename:split(Path)) of
                                    ["boss","deps"|Tail0] ->
                                        Tail = tail_or_cwd(Tail0),
                                        Path1 = filename:join(lists:reverse(Tail)),
                                        MainEbin1 = filename:join([Path1, "deps/boss/ebin"]),
                                        filelib:ensure_dir(MainEbin1++"/"),
                                        [MainEbin1|all_ebin_dirs2(Path1)];
                                    _ ->
                                        all_ebin_dirs1(Path, EbinDirs)
                                end
                        end
                            end, [], lists:reverse(BossConf)),
    lists:sort(sets:to_list(sets:from_list(BossAppEbinDir ++ MoreEbins))).

all_deps_ebin_dirs(AppFile)->
    filelib:wildcard("./deps/*/ebin").

all_boss_app_ebin_dirs(BossConf, AppFile) ->
    Boss = proplists:get_value(boss, BossConf),
    BossApp = proplists:get_value(applications, Boss),
    BinDir= fun(X) ->
                    Conf = proplists:get_value(X, BossConf),
                    case Conf of 
                        undefined -> 
                            rebar_log:log(error, 
                                          "config of your BossApp ~p is missing~n", 
                                          [X]);
                        _ ->
                            case {proplists:get_value(path, Conf), app_name(AppFile) =:= X} of
                                {undefined, true} ->
                                    filename:join(["..", X, "ebin"]);
                                {undefined, false} ->
                                    filename:join(["deps", X, "ebin"]);
                                {Path, _} ->
                                    filename:join(Path, "ebin")
                            end     
                    end
            end,
    EbinDir = [BinDir(X) || X <- BossApp, is_atom(X)],
    EbinDir.


all_ebin_dirs1(Path, EbinDirs) ->    
    MainEbin = filename:join([Path, "ebin"]),
    filelib:ensure_dir(filename:join([MainEbin, "foobar"])),
    DepsEbins = case os:type() of
                    {win32, _} ->
                        case file:list_dir(filename:join([Path, "deps"])) of
                            {ok, Dirs} -> lists:map(fun(Dir) ->
                                                            filename:join([Path, "deps", Dir, "ebin"])
                                                    end, Dirs);
                            {error, _Reason} -> []
                        end;
                    _ -> [filename:join([Path, "deps", "*", "ebin"])]
                end,
    ElixirEbins = case os:type() of
                      {win32, _} ->
                          case file:list_dir(filename:join([Path, "deps", "elixir", "lib"])) of
                              {ok, ElixirLibs} -> lists:map(fun(Dir) ->
                                                                     filename:join([Path, "deps", "elixir", "lib", Dir, "ebin"])
                                                             end, ElixirLibs);
                              {error, _} -> []
                          end;
                      _ -> [filename:join([Path, "deps", "elixir", "lib", "*", "ebin"])]
                  end,
    [MainEbin | DepsEbins] ++ ElixirEbins ++ EbinDirs.

all_ebin_dirs2(Path) ->
    case file:list_dir(filename:join([Path, "deps"])) of
        {ok, Dirs} -> 
            lists:map(fun(Dir) -> 
                              filename:join([Path, "deps", Dir, "ebin"])
                      end, Dirs);
        {error, Reason} -> 
            rebar_log:log(error, "error compilation with reason ~p~n", [Reason]),
            []
    end.       


%%--------------------------------------------------------------------
%% @doc Injects the boss.conf configuration to the boss application
%% @spec init_conf(BossConf)
%% @end
%%--------------------------------------------------------------------
init_conf(BossConf) ->
    lists:map(fun(AppLine) ->
                {App, AppConf} = AppLine, 
                lists:map(fun
                        ({Conf, Val}) ->
                            application:set_env(App, Conf, Val);
                        (true) ->
                            ok
                    end, AppConf)
        end, BossConf).

%% ===================================================================
%% Internal functions
%% ===================================================================

app_config(AppFile) ->
	{ok, AppConfig} = file:consult(AppFile),
	AppConfig.

app_name(AppFile) ->
	[{application, AppName, _}] = app_config(AppFile),
	AppName.

host_name(sname) ->
	{ok, Host} = inet:gethostname(),
	Host;
host_name(name) ->
  net_adm:localhost().

get_boss_path(BossConf)->
    case boss_config_value(BossConf, boss, path) of
        {error, _}->
            filename:join("deps", "boss");
        Path ->
            Path
    end.

vm_name(BossConf, AppFile) ->
    case {boss_config_value(BossConf, boss, vm_sname, undefined),
      boss_config_value(BossConf, boss, vm_name, undefined)} of
      {undefined, undefined} -> {sname, app_name(AppFile)};
      {undefined, Name} when is_list(Name) -> {name, Name};
      {SName, _} when is_list(SName) -> {sname, SName}
    end.

wm_qualified_name({sname, Nodename}) ->
    io_lib:format("~s@~s", [Nodename, host_name(sname)]);
wm_qualified_name({name, Nodename}) ->
    io_lib:format("~s", [Nodename]).

vm_name_arg(BossConf, AppFile) ->
  vm_name_arg(BossConf, AppFile, "").

vm_name_arg(BossConf, AppFile, Prefix) ->
  case vm_name(BossConf, AppFile) of
    {sname, SName} -> io_lib:format("-sname ~s~s", [Prefix, SName]);
    {name, Name} -> io_lib:format("-name ~s~s", [Prefix, Name])
  end.


vm_args(BossConf) ->
    case boss_config_value(BossConf, boss, vm_args) of
        {error, _} ->
            "";
        VmArgs ->
            " "++VmArgs
    end.

cookie_option(BossConf) ->
    case boss_config_value(BossConf, boss, vm_cookie) of
        {error, _} ->
            "";
      Cookie ->
        "-setcookie "++Cookie
    end.


max_processes(BossConf) ->
    boss_config_value(BossConf, boss, vm_max_processes, 32768).

erl_command() ->
    case os:type() of 
        {win32, _} -> "werl";
        _ -> "exec erl"
    end.

report_bad_client_version_and_exit(BossConf) ->
    io:format("ERROR: Your boss_rebar plugin is outdated~nPlease copy it again from your updated ChicagoBoss installation:~nGuessed command:~ncp ~s/skel/priv/rebar/boss_plugin.erl priv/rebar/boss_plugin.erl~n", [boss_config_value(BossConf, boss, path)]), 
    halt(1).

report_old_erlang_version_and_exit(Vsn) ->
    io:format("ERROR: Your Erlang version is too old. Required at least ~s, found ~s\n ", [?ERLANG_MIN_VERSION, Vsn]),
    halt(1).

tail_or_cwd([]) ->
    {ok, Path} = file:get_cwd(),
    lists:reverse(filename:split(Path));
tail_or_cwd(Tail) ->
    Tail.

