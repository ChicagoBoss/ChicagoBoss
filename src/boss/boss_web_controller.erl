-module(boss_web_controller).
-behaviour(gen_server).
-export([start_link/0, start_link/1, handle_request/4, process_request/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(DEBUGPRINT(A), error_logger:info_report("~~o)> " ++ A)).

-record(state, {
        applications = [],
        http_pid,
        smtp_pid,
        is_master_node = false
    }).

-include("boss_web.hrl").

start_link() ->
    start_link([]).

start_link(Config) ->
    gen_server:start_link({local, boss_web}, ?MODULE, Config, []).

terminate(Reason, #state{ is_master_node = true } = State) ->
    boss_news:stop(),
    boss_mq:stop(),
    boss_session:stop(),
    case boss_env:get_env(smtp_server_enable, false) of
        true ->
            gen_smtp_server:stop({global, boss_smtp_server});
        _ ->
            ok
    end,
    terminate(Reason, State#state{ is_master_node = false });
terminate(_Reason, State) ->
    lists:map(fun(AppInfo) ->
                stop_init_scripts(AppInfo#boss_app_info.application, AppInfo#boss_app_info.init_data)
        end, State#state.applications),
    error_logger:logfile(close),
    boss_translator:stop(),
    boss_router:stop(),
    boss_db:stop(),
    boss_cache:stop(),
    mochiweb_http:stop(),
    misultin:stop().

init(Config) ->
    LogDir = boss_env:get_env(log_dir, "log"),
    LogFile = make_log_file_name(LogDir),
    error_logger:logfile(close),
    ok = error_logger:logfile({open, LogFile}),
    %ok = error_logger:tty(false),
    ok = make_log_file_symlink(LogFile),

    Env = boss_env:setup_boss_env(),
    error_logger:info_msg("Starting Boss in ~p mode....~n", [Env]),
	
    boss_db:start(),

    case boss_env:get_env(cache_enable, false) of
        false -> ok;
        true -> boss_cache:start()
    end,

    boss_session:start(),

    ThisNode = erlang:node(),
    MasterNode = boss_env:get_env(master_node, ThisNode),
    if MasterNode =:= ThisNode ->
            error_logger:info_msg("Starting master services on ~p~n", [MasterNode]),
            boss_mq:start(),

            boss_news:start(),

            case boss_env:get_env(smtp_server_enable, false) of
                true ->
                    Options = [
                        {domain, boss_env:get_env(smtp_server_domain, "localhost")},
                        {address, boss_env:get_env(smtp_server_address, {0, 0, 0, 0})},
                        {port, boss_env:get_env(smtp_server_port, 25)},
                        {protocol, boss_env:get_env(smtp_server_protocol, tcp)},
                        {sessionoptions, [{boss_env, Env}]}],
                    gen_smtp_server:start({global, boss_smtp_server}, boss_smtp_server, [Options]);
                _ ->
                    ok
            end;
        true ->
            error_logger:info_msg("Pinging master node ~p from ~p~n", [MasterNode, ThisNode]),
            pong = net_adm:ping(MasterNode)
    end,

    MailDriver = boss_env:get_env(mail_driver, boss_mail_driver_smtp),
    boss_mail:start([{driver, MailDriver}]),

    ServList=boss_env:get_env(servers, [
        [{engine, boss_env:get_env(server, misultin)}, {ip, proplists:get_value(ip, Config)}, {port, proplists:get_value(port, Config)},
         {name, default},
        {ssl_enable,boss_env:get_env(ssl_enable, false)}, {ssl_options,boss_env:get_env(ssl_options, [])}]]),
    error_logger:info_msg("Configured Listener Instances: ~p~n", [ServList]),
    StartServFun =fun (ServerParams) ->
        {ServerMod, RequestMod, ResponseMod} = case proplists:get_value(engine, ServerParams) of
            mochiweb -> {mochiweb_http, mochiweb_request_bridge, mochiweb_response_bridge};
            misultin -> {misultin, misultin_request_bridge, misultin_response_bridge}
        end,
        Ip=proplists:get_value(ip, ServerParams),
        Port=proplists:get_value(port, ServerParams),
        Name=proplists:get_value(name, ServerParams),
        SSLEnable = proplists:get_value(ssl_enable, ServerParams),
        SSLOptions = proplists:get_value(ssl_options, ServerParams),
        ServerConfig = [{loop, fun(Req) -> 
                        ?MODULE:handle_request(Name, Req, RequestMod, ResponseMod)
                    end} | [{ip,Ip},{port,Port},{name,Name}]],
        error_logger:info_msg("Starting Instance: ~p ~p on ~p:~p ssl:~p ssl_opts:~p~n", [Name,ServerMod,Ip,Port,SSLEnable,SSLOptions]),
        Pid = case ServerMod of
            mochiweb_http -> mochiweb_http:start([{ssl, SSLEnable}, {ssl_opts, SSLOptions} | ServerConfig]);
            misultin -> 
                case SSLEnable of
                    true -> misultin:start_link([{ssl, SSLOptions} | ServerConfig]);
                    false -> misultin:start_link(ServerConfig)
                end
            end,
        error_logger:info_msg("Started Instance: ~p on ~p:~p ssl:~p ssl_opts:~p  pid:~p~n", [ServerMod,Ip,Port,SSLEnable,SSLOptions,Pid]),
        Pid
    end,
    Pids=lists:map(StartServFun,ServList),
    {ok, #state{ http_pid = Pids, is_master_node = (ThisNode =:= MasterNode) }, 0}.

handle_info(timeout, State) ->
    Applications = boss_env:get_env(applications, []),
    AppInfoList = lists:map(fun
            (AppName) ->
                application:start(AppName),
                BaseURL = boss_env:get_env(AppName, base_url, "/"),
                ModelList = boss_files:model_list(AppName),
                ControllerList = boss_files:web_controller_list(AppName),
                {ok, RouterSupPid} = boss_router:start([{application, AppName},
                        {controllers, ControllerList}]),
                {ok, TranslatorSupPid} = boss_translator:start([{application, AppName}]),
                case boss_env:is_developing_app(AppName) of
                    true -> boss_load:load_all_modules(AppName, TranslatorSupPid);
                    false -> ok
                end,
                InitData = run_init_scripts(AppName), 
                #boss_app_info{ application = AppName,
                    init_data = InitData,
                    router_sup_pid = RouterSupPid,
                    translator_sup_pid = TranslatorSupPid,
                    base_url = (if BaseURL =:= "/" -> ""; true -> BaseURL end),
                    model_modules = ModelList,
                    controller_modules = ControllerList
                }
        end, Applications),

    {noreply, State#state{ applications = AppInfoList }}.

handle_call({reload_translation, Locale}, _From, State) ->
    lists:map(fun(AppInfo) ->
                [{_, TranslatorPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.translator_sup_pid),
                boss_translator:reload(TranslatorPid, Locale)
        end, State#state.applications),
    {reply, ok, State};
handle_call(reload_all_translations, _From, State) ->
    lists:map(fun(AppInfo) ->
                [{_, TranslatorPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.translator_sup_pid),
                boss_translator:reload_all(TranslatorPid)
        end, State#state.applications),
    {reply, ok, State};
handle_call(reload_routes, _From, State) ->
    lists:map(fun(AppInfo) ->
                [{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
                boss_router:reload(RouterPid)
        end, State#state.applications),
    {reply, ok, State};
handle_call(reload_init_scripts, _From, State) ->
    NewApplications = lists:map(fun(AppInfo) ->
                stop_init_scripts(AppInfo#boss_app_info.application, AppInfo#boss_app_info.init_data),
                NewInitData = run_init_scripts(AppInfo#boss_app_info.application),
                AppInfo#boss_app_info{ init_data = NewInitData }
        end, State#state.applications),
    {reply, ok, State#state{ applications = NewApplications }};
handle_call(get_all_routes, _From, State) ->
    Routes = lists:map(fun(AppInfo) ->
                [{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
                {AppInfo#boss_app_info.application, boss_router:get_all(RouterPid)}
        end, State#state.applications),
    {reply, Routes, State};
handle_call(get_all_models, _From, State) ->
    Models = lists:foldl(fun(AppInfo, Acc) ->
                boss_files:model_list(AppInfo#boss_app_info.application) ++ Acc
        end, [], State#state.applications),
    {reply, Models, State};
handle_call(get_all_applications, _From, State) ->
    Applications = lists:map(fun(AppInfo) -> AppInfo#boss_app_info.application end, State#state.applications),
    {reply, Applications, State};
handle_call({translator_pid, App}, _From, State) ->
    Pid = lists:foldl(fun
            (#boss_app_info{ application = App1, translator_sup_pid = SupPid }, _) when App1 =:= App ->
                [{_, TranslatorPid, _, _}] = supervisor:which_children(SupPid),
                TranslatorPid;
            (_, Res) ->
                Res
        end, undefined, State#state.applications),
    {reply, Pid, State};
handle_call({router_pid, App}, _From, State) ->
    Pid = lists:foldl(fun
            (#boss_app_info{ application = App1, router_sup_pid = SupPid }, _) when App1 =:= App ->
                [{_, RouterPid, _, _}] = supervisor:which_children(SupPid),
                RouterPid;
            (_, Res) ->
                Res
        end, undefined, State#state.applications),
    {reply, Pid, State};
handle_call({application_info, App}, _From, State) ->
    AppInfo = lists:keyfind(App, 2, State#state.applications),
    {reply, AppInfo, State};
handle_call({base_url, App}, _From, State) ->
    BaseURL = lists:foldl(fun
            (#boss_app_info{ application = App1, base_url = URL }, _) when App1 =:= App ->
                URL;
            (_, Res) ->
                Res
        end, "", State#state.applications),
    {reply, BaseURL, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_application_for_path(Path, Applications) ->
    find_application_for_path(Path, undefined, Applications, -1).

find_application_for_path(_Path, Default, [], _LongestMatch) ->
    Default;
find_application_for_path(Path, Default, [App|Rest], LongestMatch) ->
    BaseURL = boss_web:base_url(App),
    case length(BaseURL) > LongestMatch of
        true ->
            case lists:prefix(BaseURL, Path) of
                true -> find_application_for_path(Path, App, Rest, length(BaseURL));
                false -> find_application_for_path(Path, Default, Rest, LongestMatch)
            end;
        false ->
            find_application_for_path(Path, Default, Rest, LongestMatch)
    end.

stop_init_scripts(Application, InitData) ->
    lists:foldr(fun(File, _) ->
                case boss_compiler:compile(File, []) of
                    {ok, Module} ->
                        case proplists:get_value(Module, InitData, init_failed) of
                           init_failed ->
                               ok;
                           ScriptInitData ->
                               catch Module:stop(ScriptInitData)
                       end;
                   Error -> error_logger:error_msg("Compilation of ~p failed: ~p~n", [File, Error])
                end
        end, ok, boss_files:init_file_list(Application)).

run_init_scripts(AppName) ->
    lists:foldl(fun(File, Acc) ->
                case boss_compiler:compile(File, []) of
                    {ok, Module} ->
                        case catch Module:init() of
                            {ok, Info} ->
                                [{Module, Info}|Acc];
                            ok ->
                                [{Module, true}|Acc];
                            Error ->
                                error_logger:error_msg("Execution of ~p failed: ~p~n", [File, Error]),
                                Acc
                        end;
                    Error ->
                        error_logger:error_msg("Compilation of ~p failed: ~p~n", [File, Error]),
                        Acc
                end
        end, [], boss_files:init_file_list(AppName)).

handle_request(Instance, Req, RequestMod, ResponseMod) ->
    LoadedApplications = boss_web:get_all_applications(),
    Request = simple_bridge:make_request(RequestMod, Req),
    case Request:path() of
        FullUrl ->
            App = find_application_for_path(Request:path(), LoadedApplications),
            BaseURL = boss_web:base_url(App),
            DocRoot = boss_files:static_path(App),
            Url = lists:nthtail(length(BaseURL), FullUrl),
            case Url of
                "/favicon.ico" = File ->
                    Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
                    (Response:file(File)):build_response();
                "/static/"++File -> 
                    Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
                    (Response:file([$/|File])):build_response();
                _ -> 
                    SessionKey = boss_session:get_session_key(),
                    SessionID = case boss_env:get_env(session_enable, true) of
                        true ->
                            boss_session:new_session(Request:cookie(SessionKey));
                        false ->
                            undefined
                    end,
                    Mode = case boss_env:is_developing_app(App) of
                        true -> development;
                        false -> production
                    end,
                    AppInfo = boss_web:application_info(App),
                    TranslatorPid = boss_web:translator_pid(App),
                    RouterPid = boss_web:router_pid(App),
                    {Time, {StatusCode, Headers, Payload}} = timer:tc(?MODULE, process_request, [
                        AppInfo#boss_app_info{ translator_pid = TranslatorPid, router_pid = RouterPid }, 
                        Request, Mode, Instance, Url, SessionID]),
                    ErrorFormat = "~s ~s [~p] ~p ~pms~n", 
                    ErrorArgs = [Request:request_method(), Request:path(), App, StatusCode, Time div 1000],
                    case StatusCode of
                        500 -> error_logger:error_msg(ErrorFormat, ErrorArgs);
                        404 -> error_logger:warning_msg(ErrorFormat, ErrorArgs);
                        _ -> error_logger:info_msg(ErrorFormat, ErrorArgs)
                    end,
                    Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
                    Response1 = (Response:status_code(StatusCode)):data(Payload),
                    Response2 = case SessionID of
                        undefined ->
                            Response1;
                        _ ->
                            SessionExpTime = boss_session:get_session_exp_time(),
                            Response1:cookie(SessionKey, SessionID, "/", SessionExpTime)
                    end,
                    Response3 = lists:foldl(fun({K, V}, Acc) -> Acc:header(K, V) end, Response2, Headers),
                    Response3:build_response()
            end
    end.

process_request(AppInfo, Req, development, Instance, "/doc", SessionID) ->
    Result = case catch load_and_execute(development, {"doc", [], []}, AppInfo, Instance, Req, SessionID) of
        {'EXIT', Reason} ->
            {error, Reason};
        Ok ->
            Ok
    end,
    process_result(AppInfo, Result);
process_request(AppInfo, Req, development, Instance, "/doc/"++ModelName, SessionID) ->
    Result = case string:chr(ModelName, $.) of
        0 ->
            case catch load_and_execute(development, {"doc", ModelName, []}, AppInfo, Instance, Req, SessionID) of
                {'EXIT', Reason} ->
                    {error, Reason};
                Ok ->
                    Ok
            end;
        _ ->
            {not_found, "File not found"}
    end,
    process_result(AppInfo, Result);
process_request(AppInfo, Req, Mode, Instance, Url, SessionID) ->
    RouterPid = AppInfo#boss_app_info.router_pid,
    if 
        Mode =:= development ->
            ControllerList = boss_files:web_controller_list(AppInfo#boss_app_info.application),
            boss_router:set_controllers(RouterPid, ControllerList);
        true ->
            ok
    end,
    Location = case boss_router:route(RouterPid, Url) of
        {ok, {Controller, Action, Tokens}} ->
            {Controller, Action, Tokens};
        not_found ->
            case boss_router:handle(RouterPid, 404) of
                {ok, {Controller, Action, Tokens}} ->
                    {Controller, Action, Tokens};
                not_found ->
                    undefined
            end
    end,
    Result = case Location of
        undefined ->
            {not_found, "The requested page was not found. Additionally, no handler was found for processing 404 errors."};
        _ ->
            case catch load_and_execute(Mode, Location, AppInfo, Instance, Req, SessionID) of
                {'EXIT', Reason} ->
                    {error, Reason};
                Ok ->
                    Ok
            end
    end,
    process_result(AppInfo, Result).

process_result(_, {error, Payload}) ->
    error_logger:error_report(Payload),
    {500, [{"Content-Type", "text/html"}], "Error: <pre>" ++ io_lib:print(Payload) ++ "</pre>"};
process_result(_, {not_found, Payload}) ->
    {404, [{"Content-Type", "text/html"}], Payload};
process_result(AppInfo, {redirect, Where}) ->
	process_result(AppInfo, {redirect, Where, []});
process_result(AppInfo, {redirect, "http://"++Where, Headers}) ->
    process_result(AppInfo, {redirect_external, "http://"++Where, Headers});
process_result(AppInfo, {redirect, "https://"++Where, Headers}) ->
    process_result(AppInfo, {redirect_external, "https://"++Where, Headers});
process_result(_AppInfo, {redirect, {Application, Controller, Action, Params}, Headers}) ->
    RouterPid = boss_web:router_pid(list_to_atom(lists:concat([Application]))),
    URL = boss_router:unroute(RouterPid, Controller, Action, Params),
    BaseURL = boss_web:base_url(list_to_atom(lists:concat([Application]))),
    {302, [{"Location", BaseURL ++ URL}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(AppInfo, {redirect, Where, Headers}) ->
    {302, [{"Location", AppInfo#boss_app_info.base_url ++ Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(_, {redirect_external, Where, Headers}) ->
    {302, [{"Location", Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(_, {ok, Payload, Headers}) ->
    {200, [{"Content-Type", proplists:get_value("Content-Type", Headers, "text/html")}
            |proplists:delete("Content-Type", Headers)], Payload}.

<<<<<<< HEAD
load_and_execute(production, {Controller, _, _} = Location, AppInfo, Instance, Req, SessionID) ->
=======
load_and_execute(Mode, {Controller, _, _} = Location, AppInfo, Req, SessionID) when Mode =:= production; Mode =:= testing->
>>>>>>> 35647bade37ccab697e23b2256d0259c940e4b51
    case lists:member(boss_files:web_controller(AppInfo#boss_app_info.application, Controller), 
            AppInfo#boss_app_info.controller_modules) of
        true -> execute_action(Location, AppInfo, Instance, Req, SessionID);
        false -> render_view(Location, AppInfo, Req, SessionID)
    end;
load_and_execute(development, {"doc", ModelName, _}, AppInfo, _Instance, Req, _SessionID) ->
    case boss_load:load_models() of
        {ok, ModelModules} ->
            case lists:member(ModelName, lists:map(fun atom_to_list/1, ModelModules)) of
                true ->
                    Model = list_to_atom(ModelName),
                    {Model, Edoc} = boss_record_compiler:edoc_module(
                        boss_files:model_path(ModelName++".erl"), [{private, true}]),
                    {ok, edoc:layout(Edoc), []};
                false ->
                    case boss_html_doc_template:render([
                                {application, AppInfo#boss_app_info.application},
                                {'_base_url', AppInfo#boss_app_info.base_url},
                                {models, ModelModules}]) of
                        {ok, Payload} ->
                            {ok, Payload, []};
                        Err ->
                            Err
                    end
            end;
        {error, ErrorList} ->
            render_errors(ErrorList, AppInfo, Req)
    end;
load_and_execute(development, {Controller, _, _} = Location, AppInfo, Instance, Req, SessionID) ->
    case boss_load:load_mail_controllers() of
        {ok, _} -> 
            case boss_load:load_libraries() of
                {ok, _} ->
                    case boss_load:load_web_controllers() of
                        {ok, Controllers} ->
                            case lists:member(boss_files:web_controller(AppInfo#boss_app_info.application, Controller), 
                                    lists:map(fun atom_to_list/1, Controllers)) of
                                true ->
                                    case boss_load:load_models() of
                                        {ok, _} ->
                                            execute_action(Location, AppInfo, Instance, Req, SessionID);
                                        {error, ErrorList} ->
                                            render_errors(ErrorList, AppInfo, Req)
                                    end;
                                false ->
                                    render_view(Location, AppInfo, Req, SessionID)
                            end;
                        {error, ErrorList} when is_list(ErrorList) ->
                            render_errors(ErrorList, AppInfo, Req)
                    end;
                {error, ErrorList} ->
                    render_errors(ErrorList, AppInfo, Req)
            end;
        {error, ErrorList} ->
            render_errors(ErrorList, AppInfo, Req)
    end.

render_errors(ErrorList, AppInfo, Req) ->
    case boss_html_error_template:render([{errors, ErrorList}, {request, Req}, 
                {application, AppInfo#boss_app_info.application}]) of
        {ok, Payload} ->
            {ok, Payload, []};
        Err ->
            Err
    end.

execute_action(Location, AppInfo, Instance, Req, SessionID) ->
    execute_action(Location, AppInfo, Instance, Req, SessionID, []).

execute_action({Controller, Action}, AppInfo, Instance, Req, SessionID, LocationTrail) ->
    execute_action({Controller, Action, []}, AppInfo, Instance, Req, SessionID, LocationTrail);
execute_action({Controller, Action, Tokens}, AppInfo, Instance, Req, SessionID, LocationTrail) when is_atom(Action) ->
    execute_action({Controller, atom_to_list(Action), Tokens}, AppInfo, Instance, Req, SessionID, LocationTrail);
execute_action({Controller, Action, Tokens} = Location, AppInfo, Instance, Req, SessionID, LocationTrail) ->
    case lists:member(Location, LocationTrail) of
        true ->
            {error, "Circular redirect!"};
        false ->
            % do not convert a list to an atom until we are sure the controller/action
            % pair exists. this prevents a memory leak due to atom creation.
            Module = list_to_atom(boss_files:web_controller(AppInfo#boss_app_info.application, Controller)),
            ExportStrings = lists:map(
                fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
                Module:module_info(exports)),
            ControllerInstance = case proplists:get_value("new", ExportStrings) of
                2 -> 
                    Module:new(Instance, Req);
                3 ->
                    Module:new(Instance, Req, SessionID)
            end,
            AuthInfo = case lists:member({"before_", 2}, ExportStrings) of
                true -> 
                    case ControllerInstance:before_(Action) of
                        ok ->
                            {ok, undefined};
                        OtherInfo ->
                            OtherInfo
                    end;
                false -> 
                    {ok, undefined}
            end,
            case AuthInfo of
                {ok, Info} ->
                   ActionResult = case proplists:get_value(Action, ExportStrings) of
                        3 ->
                            ActionAtom = list_to_atom(Action),
                            ControllerInstance:ActionAtom(Req:request_method(), Tokens);
                        4 ->
                            ActionAtom = list_to_atom(Action),
                            ControllerInstance:ActionAtom(Req:request_method(), Tokens, Info);
                        _ ->
                            undefined
                    end,
                    Result = case ActionResult of
                        undefined ->
                            render_view(Location, AppInfo, Req, SessionID, [{"_before", Info}]);
                        ActionResult ->
                            process_action_result({Location, Req, SessionID, [Location|LocationTrail]}, 
                                ActionResult, AppInfo, Info)
                    end,
                    case proplists:get_value("after_", ExportStrings) of
                        3 ->
                            ControllerInstance:after_(Action, Result);
                        4 ->
                            ControllerInstance:after_(Action, Result, Info);
                        _ ->
                            Result
                    end;
                {redirect, Where} ->
                    {redirect, process_redirect(Controller, Where, AppInfo)}
            end
    end.

process_location(Controller,  [{_, _}|_] = Where, AppInfo) ->
    {_, TheController, TheAction, CleanParams} = process_redirect(Controller, Where, AppInfo),
    ControllerModule = list_to_atom(boss_files:web_controller(AppInfo#boss_app_info.application, Controller)),
    ActionAtom = list_to_atom(TheAction),
    {Tokens, []} = boss_controller_lib:convert_params_to_tokens(CleanParams, ControllerModule, ActionAtom),
    {TheController, TheAction, Tokens}.

process_redirect(Controller, [{_, _}|_] = Where, AppInfo) ->
    TheApplication = proplists:get_value(application, Where, AppInfo#boss_app_info.application),
    TheController = proplists:get_value(controller, Where, Controller),
    TheAction = proplists:get_value(action, Where),
    CleanParams = lists:foldl(fun(Key, Vars) ->
                proplists:delete(Key, Vars)
        end, Where, [application, controller, action]),
    {TheApplication, TheController, TheAction, CleanParams};
process_redirect(_, Where, _) ->
    Where.

process_action_result(Info, ok, AppInfo, AuthInfo) ->
    process_action_result(Info, {ok, []}, AppInfo, AuthInfo);
process_action_result(Info, {ok, Data}, AppInfo, AuthInfo) ->
    process_action_result(Info, {ok, Data, []}, AppInfo, AuthInfo);
process_action_result({Location, Req, SessionID, _}, {ok, Data, Headers}, AppInfo, AuthInfo) ->
    render_view(Location, AppInfo, Req, SessionID, [{"_before", AuthInfo}|Data], Headers);

process_action_result(Info, {render_other, OtherLocation}, AppInfo, AuthInfo) ->
    process_action_result(Info, {render_other, OtherLocation, []}, AppInfo, AuthInfo);
process_action_result(Info, {render_other, OtherLocation, Data}, AppInfo, AuthInfo) ->
    process_action_result(Info, {render_other, OtherLocation, Data, []}, AppInfo, AuthInfo);
process_action_result({{Controller, _, _}, Req, SessionID, _}, {render_other, OtherLocation, Data, Headers}, AppInfo, AuthInfo) ->
    render_view(process_location(Controller, OtherLocation, AppInfo),
        AppInfo, Req, SessionID, [{"_before", AuthInfo}|Data], Headers);

process_action_result({{Controller, _, _}, Req, SessionID, LocationTrail}, {action_other, OtherLocation}, AppInfo, _) ->
    execute_action(process_location(Controller, OtherLocation, AppInfo), AppInfo, Req, SessionID, LocationTrail);

process_action_result({_, Req, SessionID, LocationTrail}, not_found, AppInfo, _) ->
    NotFoundLocation = boss_router:handle(AppInfo#boss_app_info.router_pid, 404),
    execute_action(NotFoundLocation, AppInfo, Req, SessionID, LocationTrail);

process_action_result(Info, {redirect, Where}, AppInfo, AuthInfo) ->
    process_action_result(Info, {redirect, Where, []}, AppInfo, AuthInfo);
process_action_result({{Controller, _, _}, _, _, _}, {redirect, Where, Headers}, AppInfo, _) ->
    {redirect, process_redirect(Controller, Where, AppInfo), Headers};

process_action_result(Info, {json, Data}, AppInfo, AuthInfo) ->
    process_action_result(Info, {json, Data, []}, AppInfo, AuthInfo);
process_action_result(Info, {json, Data, Headers}, AppInfo, AuthInfo) ->
    process_action_result(Info, {output, boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
            [{"Content-Type", proplists:get_value("Content-Type", Headers, "application/json")}
                |proplists:delete("Content-Type", Headers)]}, AppInfo, AuthInfo);

process_action_result(Info, {jsonp, Callback, Data}, AppInfo, AuthInfo) ->
    process_action_result(Info, {jsonp, Callback, Data, []}, AppInfo, AuthInfo);
process_action_result(Info, {jsonp, Callback, Data, Headers}, AppInfo, AuthInfo) ->
    JsonData  = boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
    process_action_result(Info, {output, Callback ++ "(" ++ JsonData ++ ");",
            [{"Content-Type", proplists:get_value("Content-Type", Headers, "application/javascript")}
                |proplists:delete("Content-Type", Headers)]}, AppInfo, AuthInfo);

process_action_result(Info, {output, Payload}, AppInfo, AuthInfo) ->
    process_action_result(Info, {output, Payload, []}, AppInfo, AuthInfo);
process_action_result(_, {output, Payload, Headers}, _, _) ->
    {ok, Payload, Headers};

process_action_result(_, Else, _, _) ->
    Else.

render_view(Location, AppInfo, Req, SessionID) ->
    render_view(Location, AppInfo, Req, SessionID, []).

render_view(Location, AppInfo, Req, SessionID, Variables) ->
    render_view(Location, AppInfo, Req, SessionID, Variables, []).

render_view({Controller, Template, _}, AppInfo, Req, SessionID, Variables, Headers) ->
    ViewPath = boss_files:web_view_path(Controller, Template),
    LoadResult = boss_load:load_view_if_dev(AppInfo#boss_app_info.application, ViewPath, AppInfo#boss_app_info.translator_pid),
    BossFlash = boss_flash:get_and_clear(SessionID),
    case LoadResult of
        {ok, Module} ->
            {Lang, TranslationFun} = choose_translation_fun(AppInfo#boss_app_info.translator_pid, 
                Module:translatable_strings(), Req:header(accept_language), 
                proplists:get_value("Content-Language", Headers)),
            case Module:render(lists:merge([{"_lang", Lang}, 
                            {"_base_url", AppInfo#boss_app_info.base_url}|Variables], BossFlash), 
                    [{translation_fun, TranslationFun}, {locale, Lang},
                        {custom_tags_context, [{controller, Controller}, 
                                {application, atom_to_list(AppInfo#boss_app_info.application)},
                                {action, Template}]}]) of
                {ok, Payload} ->
                    {ok, Payload, Headers};
                Err ->
                    Err
            end;
        {error, not_found} ->
            {not_found, io_lib:format("The requested template (~p) was not found.", [ViewPath]) };
        {error, {File, [{0, _Module, "Failed to read file"}]}} ->
            {not_found, io_lib:format("The requested template (~p) was not found.", [File]) };
        {error, Error}-> 
            render_errors([Error], AppInfo, Req)
    end.

choose_translation_fun(_, _, undefined, undefined) ->
    DefaultLang = boss_env:get_env(assume_locale, "en"),
    {DefaultLang, none};
choose_translation_fun(TranslatorPid, Strings, AcceptLanguages, undefined) ->
    DefaultLang = boss_env:get_env(assume_locale, "en"),
    case mochiweb_util:parse_qvalues(AcceptLanguages) of
        invalid_qvalue_string ->
            {DefaultLang, none};
        [{Lang, _}] ->
            {Lang, boss_translator:fun_for(TranslatorPid, Lang)};
        QValues when length(QValues) > 1 ->
            {BestLang, BestNetQValue} = choose_language_from_qvalues(TranslatorPid, Strings, QValues),
            case BestNetQValue of
                0.0 -> {DefaultLang, none};
                _ -> {BestLang, boss_translator:fun_for(TranslatorPid, BestLang)}
            end
    end;
choose_translation_fun(TranslatorPid, _, _, ContentLanguage) ->
    {ContentLanguage, boss_translator:fun_for(TranslatorPid, ContentLanguage)}.

choose_language_from_qvalues(TranslatorPid, Strings, QValues) ->
    % calculating translation coverage is costly so we start with the most preferred
    % languages and work our way down
    SortedQValues = lists:reverse(lists:keysort(2, QValues)),
    AssumedLocale = boss_env:get_env(assume_locale, "en"),
    AssumedLocaleQValue = proplists:get_value(AssumedLocale, SortedQValues, 0.0),
    lists:foldl(
        fun
            ({_, ThisQValue}, {BestLang, BestTranslationScore}) when BestTranslationScore >= ThisQValue ->
                {BestLang, BestTranslationScore};
            ({ThisLang, ThisQValue}, {_, BestTranslationScore}) when ThisLang =:= AssumedLocale andalso
                                                                     ThisQValue > BestTranslationScore ->
                {ThisLang, ThisQValue}; % translation coverage is 100%
            ({ThisLang, ThisQValue}, {BestLang, BestTranslationScore}) ->
                TranslationCoverage = translation_coverage(Strings, ThisLang, TranslatorPid),
                TranslationScore = ThisQValue * TranslationCoverage + 
                                    AssumedLocaleQValue * (1-TranslationCoverage),
                case TranslationScore > BestTranslationScore of
                    true -> {ThisLang, TranslationScore};
                    false -> {BestLang, BestTranslationScore}
                end
        end, {"xx-bork", 0.0}, SortedQValues).

translation_coverage([], _, _) ->
    0.0;
translation_coverage(Strings, Locale, TranslatorPid) ->
    case boss_translator:is_loaded(TranslatorPid, Locale) of
        true ->
            lists:foldl(fun(String, Acc) ->
                        case boss_translator:lookup(TranslatorPid, String, Locale) of
                            undefined -> Acc;
                            _ -> Acc + 1
                        end
                end, 0, Strings) / length(Strings);
        false ->
            0.0
    end.

make_log_file_name(Dir) ->
    {{Y, M, D}, {Hour, Min, Sec}} = calendar:local_time(), 
    filename:join([Dir, 
            lists:flatten(io_lib:format("boss_error-~4..0B-~2..0B-~2..0B.~2..0B-~2..0B-~2..0B.log", 
                    [Y, M, D, Hour, Min, Sec]))]).


make_log_file_symlink(LogFile) ->
    SymLink = filename:join([filename:dirname(LogFile), "boss_error-LATEST.log"]),
     case os:type() of
        {unix,_} ->
            file:delete(SymLink),
            file:make_symlink(filename:basename(LogFile), SymLink);
        {win32,_} ->
            file:delete(SymLink),
            {ok, Cwd} = file:get_cwd(),
            LinkTarget = Cwd ++ "/log/" ++ filename:basename(LogFile),
            mk_win_dir_syslink("boss_error-LATEST.log", filename:dirname(LogFile), LinkTarget)
     end.

%% @doc Make symbolik link in current directory on windows vista or highter
mk_win_dir_syslink(LinkName, DestDir, LinkTarget) ->
    S = (list_to_atom(lists:append(["cd ", DestDir, "& mklink ", LinkName, " \"", LinkTarget, "\""]))),
    os:cmd(S),
    ok.
