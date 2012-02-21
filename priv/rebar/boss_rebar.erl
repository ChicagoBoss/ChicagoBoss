%%%-------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% @doc Chicago Boss rebar functions, called from boss_plugin
%%%  Managing compilation/configuration/scripts stuff, the boss way
%%% @end
%%%-------------------------------------------------------------------
-module(boss_rebar).

-export([run/4, 
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
		 boss_config_value/3,
		 boss_config_value/4,
		 boss_load/2,
		 boss_start/1,
		 all_ebin_dirs/2]).

-define(COMMANDS, 
		[{help,  "Lists all commands"},
		 {compile, "Compile (boss way)"},
		 {test_eunit, "Run src/test/eunit tests"},
		 {test_functional, "Run src/test/functional tests"},
		 {start_cmd, "Generates the start shell command"},
		 {start_dev_cmd, "Generates the start-dev shell command"},
		 {stop_cmd, "Generates the stop shell command"},
		 {reload_cmd, "Generates the hot reload shell command"}
		]).

%%--------------------------------------------------------------------
%% @doc run
%% @spec run(Command, RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Rebar Commands Router
%% @end
%%--------------------------------------------------------------------
run(Command, RebarConf, BossConf, AppFile) when is_list(Command)->
	run(list_to_atom(Command), RebarConf, BossConf, AppFile);
run(Command, RebarConf, BossConf, AppFile) ->
	rebar_log:log(debug, "About to run command '~s'~n", [Command]),
	case lists:keyfind(Command, 1, ?COMMANDS) of
		false -> 
			{error, command_not_found};
		_ -> 
			apply(boss_rebar, Command, [RebarConf, BossConf, AppFile])
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
compile(_RebarConf, BossConf, AppFile, Dest) ->
	boss_load(BossConf, AppFile),
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
	boss_rebar:boss_start(BossConf),
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
	SName = sname(BossConf, AppFile),
	Cookie = cookie(BossConf),

	io:format("exec erl +K true -pa ~s -boot start_sasl -config boss -s boss -setcookie ~s -detached -sname ~s", 
			  [string:join(EbinDirs, " -pa "), Cookie, SName]),
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
	
	AppName = app_name(AppFile),
	EbinDirs = all_ebin_dirs(BossConf, AppFile),
	SName = sname(BossConf, AppFile),

	io:format("exec erl -pa ~s -boss developing_app ~s -boot start_sasl -config boss -s reloader -s boss -sname ~s", 
			  [string:join(EbinDirs, " -pa "), AppName, SName]),
	ok.

%%--------------------------------------------------------------------
%% @doc stop_cmd
%% @spec stop_cmd(RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Generate stop shell command (production)
%% @end
%%--------------------------------------------------------------------
stop_cmd(_RebarConf, BossConf, AppFile) ->
	rebar_log:log(info, "Generating dynamic stop command~n", []),
	
	SName = sname(BossConf, AppFile),
	Cookie = cookie(BossConf),
	StopCommand = io_lib:format("rpc:call('~s', init, stop, []).", [SName]),
	
	io:format("erl -noshell -pa ebin -setcookie ~s -sname stopper_~s -eval \"~s\" -s init stop", 
			  [Cookie, SName, StopCommand]),
	ok.

%%--------------------------------------------------------------------
%% @doc reload_cmd
%% @spec reload_cmd(RebarConf, BossConf, AppFile) -> ok | {error, Reason}
%%       Generate hot reload shell command (production)
%% @end
%%--------------------------------------------------------------------
reload_cmd(_RebarConf, BossConf, AppFile) ->
	rebar_log:log(info, "Generating dynamic reload command~n", []),
	SName = sname(BossConf, AppFile),
	Cookie = cookie(BossConf),
	ReloadCode = io_lib:format("rpc:call('~s', boss_load, reload_all, [])", [SName]),
	ReloadRoutes = io_lib:format("rpc:call('~s', boss_web, reload_routes, [])", [SName]),
	ReloadLangs = io_lib:format("rpc:call('~s', boss_web, reload_all_translations, [])", [SName]),
	io:format("erl -noshell -pa ebin -setcookie ~s -sname reloader_~s -eval \"~s, ~s, ~s.\" -s init stop", 
			  [Cookie, SName, ReloadCode, ReloadRoutes, ReloadLangs]),
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
					  [code:load_abs(string:substr(B, 1, string:len(B) - string:len(".beam"))) || 
						B <- rebar_utils:beams(Dir)]
			  end, AllDirs).

%%--------------------------------------------------------------------
%% @doc Start the boss app
%% @spec boss_start(BossConfig)
%% @end
%%--------------------------------------------------------------------
boss_start(_BossConf) ->
	application:start(boss).

%%--------------------------------------------------------------------
%% @doc Start the boss app
%% @spec all_ebin_dirs(BossConf, _AppFile)
%%       Gets all ebin dirs for the apps defined in boss.config
%% @end
%%--------------------------------------------------------------------
all_ebin_dirs(BossConf, _AppFile) ->
	lists:foldl(fun({_App, Config}, EbinDirs) ->
						case lists:keyfind(path, 1, Config) of
							false -> EbinDirs;
							{path, Path} -> 
								AddedEbin = [ filename:join([Path, "deps", "*", "ebin"]) |EbinDirs],
								[filename:join([Path, "ebin"])|AddedEbin]
						end end, [], lists:reverse(BossConf)).

%% ===================================================================
%% Internal functions
%% ===================================================================

app_config(AppFile) ->
	{ok, AppConfig} = file:consult(AppFile),
	AppConfig.

app_name(AppFile) ->
	[{application, AppName, _}] = app_config(AppFile),
	AppName.

host_name() ->
	{ok, Host} = inet:gethostname(),
	Host.

sname(BossConf, AppFile) ->
    boss_config_value(BossConf, boss, vm_name, io_lib:format("~s@~s", [app_name(AppFile), host_name()])).

cookie(BossConf) ->
    boss_config_value(BossConf, boss, vm_cookie, "abc123").
