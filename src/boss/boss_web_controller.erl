-module(boss_web_controller).
-behaviour(gen_server).
-export([start_link/0, start_link/1, handle_request/3, process_request/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([merge_headers/2]).
-export([handle_news_for_cache/3]).
-define(DEBUGPRINT(A), error_logger:info_report("~~o)> " ++ A)).
-define(BUILTIN_CONTROLLER_FILTERS, [boss_lang_filter, boss_cache_page_filter, boss_cache_vars_filter]).
-define(DEFAULT_WEB_SERVER, cowboy).

-record(state, {
        applications = [],
        service_sup_pid,
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
    boss_model_manager:stop(),
    boss_cache:stop(),
    mochiweb_http:stop(),
    application:stop(elixir).

init(Config) ->
    case boss_env:get_env(log_enable, true) of
        false -> ok;
        true -> lager:start()
    end,

    application:start(elixir),

    Env = boss_env:setup_boss_env(),

    error_logger:info_msg("Starting Boss in ~p mode....~n", [Env]),

    boss_model_manager:start(),

    case boss_env:get_env(cache_enable, false) of
        false -> ok;
        true ->
            CacheAdapter = boss_env:get_env(cache_adapter, memcached_bin),
            CacheOptions =
                case CacheAdapter of
                    ets ->
                        MaxSize = boss_env:get_env(ets_maxsize, 32 * 1024 * 1024),
                        Threshold = boss_env:get_env(ets_threshold, 0.85),
                        Weight = boss_env:get_env(ets_weight, 30),
                        [{adapter, ets}, {ets_maxsize, MaxSize},
                         {ets_threshold, Threshold}, {ets_weight, Weight}];
                    _ ->
                        [{adapter, CacheAdapter},
                         {cache_servers, boss_env:get_env(cache_servers, [{"127.0.0.1", 11211, 1}])}]
                end,
            boss_cache:start(CacheOptions)
    end,

    boss_session:start(),

    ThisNode = erlang:node(),
    MasterNode = boss_env:master_node(),
    if
        MasterNode =:= ThisNode ->
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

    {ServerMod, RequestMod, ResponseMod} = case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
        mochiweb -> {mochiweb_http, mochiweb_request_bridge, mochiweb_response_bridge};
        cowboy -> {cowboy, mochiweb_request_bridge, mochiweb_response_bridge}
    end,
    SSLEnable = boss_env:get_env(ssl_enable, false),
    SSLOptions = boss_env:get_env(ssl_options, []),
    error_logger:info_msg("SSL:~p~n", [SSLOptions]),

    {ok, ServicesSupPid}  = case MasterNode of
				ThisNode ->
				    boss_service_sup:start_link();
				_AnyNode ->
				    {ok, undefined}
			    end,

    ServerConfig = [{loop, fun(Req) ->
                    ?MODULE:handle_request(Req, RequestMod, ResponseMod)
            end} | Config],
    Pid = case ServerMod of
        mochiweb_http ->
            application:start(mochiweb),
            mochiweb_http:start([{ssl, SSLEnable}, {ssl_opts, SSLOptions} | ServerConfig]);
        cowboy ->
		  %Dispatch = [{'_', [{'_', boss_mochicow_handler, [{loop, {boss_mochicow_handler, loop}}]}]}],
		  error_logger:info_msg("Starting cowboy... on ~p~n", [MasterNode]),
		  application:start(ranch),
		  application:start(cowboy),
		  HttpPort = boss_env:get_env(port, 8001),
          AcceptorCount = boss_env:get_env(acceptor_processes, 100),
          case SSLEnable of
              false ->
                  error_logger:info_msg("Starting http listener... on ~p ~n", [HttpPort]),
                  cowboy:start_http(boss_http_listener, AcceptorCount, [{port, HttpPort}], [{env, []}]);
              true ->
                  error_logger:info_msg("Starting https listener... on ~p ~n", [HttpPort]),
                  SSLConfig = [{port, HttpPort}]++SSLOptions,
                  cowboy:start_https(boss_https_listener, AcceptorCount, SSLConfig, [{env, []}])
		  end,
		  if
              MasterNode =:= ThisNode ->
                  boss_service_sup:start_services(ServicesSupPid, boss_websocket_router);
              true ->
                  ok
		  end

	  end,
    {ok, #state{ service_sup_pid = ServicesSupPid, http_pid = Pid, is_master_node = (ThisNode =:= MasterNode) }, 0}.

handle_info(timeout, State) ->
    Applications = boss_env:get_env(applications, []),
    ServicesSupPid = State#state.service_sup_pid,
    AppInfoList = lists:map(fun
            (AppName) ->
                application:start(AppName),
                BaseURL = boss_env:get_env(AppName, base_url, "/"),
                StaticPrefix = boss_env:get_env(AppName, static_prefix, "/static"),
                DocPrefix = boss_env:get_env(AppName, doc_prefix, "/doc"),
                DomainList = boss_env:get_env(AppName, domains, all),
                ModelList = boss_files:model_list(AppName),
                ViewList = boss_files:view_module_list(AppName),
                IsMasterNode = boss_env:is_master_node(),
                ControllerList = boss_files:web_controller_list(AppName),
                {ok, RouterSupPid} = boss_router:start([{application, AppName},
                        {controllers, ControllerList}]),
                {ok, TranslatorSupPid} = boss_translator:start([{application, AppName}]),
                case boss_env:is_developing_app(AppName) of
                    true -> boss_load:load_all_modules(AppName, TranslatorSupPid);
                    false -> ok
                end,
                
                if
                    IsMasterNode ->
                        case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
                            cowboy ->
                                WebSocketModules = boss_files:websocket_list(AppName),
                                MappingServices  = boss_files:websocket_mapping(BaseURL,
                                    atom_to_list(AppName),
                                    WebSocketModules),
                                boss_service_sup:start_services(ServicesSupPid, MappingServices);
                            _Any ->
                                _Any
                        end;
                    true ->
                        ok
                end,
                
                InitData = run_init_scripts(AppName),
                #boss_app_info{ application = AppName,
                    init_data = InitData,
                    router_sup_pid = RouterSupPid,
                    translator_sup_pid = TranslatorSupPid,
                    base_url = (if BaseURL =:= "/" -> ""; true -> BaseURL end),
                    static_prefix = StaticPrefix,
                    doc_prefix = DocPrefix,
                    domains = DomainList,
                    model_modules = ModelList,
                    view_modules = ViewList,
                    controller_modules = ControllerList
                }
        end, Applications),

    %% cowboy dispatch rule for static content
    case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
        cowboy ->
            AppStaticDispatches = lists:map(
                fun(AppName) ->
                        BaseURL = boss_env:get_env(AppName, base_url, "/"),
                        StaticPrefix = boss_env:get_env(AppName, static_prefix, "/static"),
					    Path = BaseURL++StaticPrefix,
					    Handler = cowboy_static,
					    Opts = [
                            {directory, {priv_dir, AppName, [<<"static">>]}},
						    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                        ],
                       {Path ++ "[...]", Handler, Opts}
                end, Applications),

            BossDispatch = [{'_', boss_mochicow_handler, [{loop, {boss_mochicow_handler, loop}}]}],
            % [{"/", boss_mochicow_handler, []}],
            %Dispatch = [{'_',

            Dispatch = [{'_', AppStaticDispatches ++ BossDispatch}],
            CowboyListener = case boss_env:get_env(ssl_enable, false) of
                true -> boss_https_listener;
                _ -> boss_http_listener
            end,
            cowboy:set_env(CowboyListener, dispatch, cowboy_router:compile(Dispatch));
        _Oops ->
            _Oops
    end,
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
    {reply, BaseURL, State};
handle_call({static_prefix, App}, _From, State) ->
    StaticPrefix = lists:foldl(fun
            (#boss_app_info{ application = App1, static_prefix = Prefix }, _) when App1 =:= App ->
                Prefix;
            (_, Res) ->
                Res
        end, "/static", State#state.applications),
    {reply, StaticPrefix, State};
handle_call({domains, App}, _From, State) ->
    DomainList = lists:foldl(fun
            (#boss_app_info{ application = App1, domains = Domains}, _) when App1 =:= App ->
                Domains;
            (_, Res) ->
                Res
        end, all, State#state.applications),
    {reply, DomainList, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_application_for_path(Host, Path, Applications) ->
    UseHost = case Host of
        undefined -> undefined;
        _ -> hd(re:split(Host, ":", [{return, list}]))
    end,
    find_application_for_path(UseHost, Path, undefined, Applications, -1).

find_application_for_path(_Host, _Path, Default, [], _MatchScore) ->
    Default;
find_application_for_path(Host, Path, Default, [App|Rest], MatchScore) ->
    DomainScore = case Host of
        undefined -> 0;
        _ ->
            case boss_web:domains(App) of
                all -> 0;
                Domains ->
                    case lists:member(Host, Domains) of
                        true -> 1;
                        false -> -1
                    end
            end
    end,
    BaseURL = boss_web:base_url(App),
    PathScore = length(BaseURL),
    {UseApp, UseScore} = case (DomainScore >= 0) andalso (1000 * DomainScore + PathScore > MatchScore) andalso lists:prefix(BaseURL, Path) of
        true -> {App, DomainScore * 1000 + PathScore};
        false -> {Default, MatchScore}
    end,
    find_application_for_path(Host, Path, UseApp, Rest, UseScore).

stop_init_scripts(Application, InitData) ->
    lists:foldr(fun(File, _) ->
                case boss_compiler:compile(File, [{include_dirs, [boss_files:include_dir() | boss_env:get_env(boss, include_dirs, [])]}]) of
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
                case boss_compiler:compile(File, [{include_dirs, [boss_files:include_dir() | boss_env:get_env(boss, include_dirs, [])]}]) of
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

handle_request(Req, RequestMod, ResponseMod) ->
	LoadedApplications = boss_web:get_all_applications(),
	Request = simple_bridge:make_request(RequestMod, Req),
	FullUrl = Request:path(),
	case find_application_for_path(Request:header(host), FullUrl, LoadedApplications) of
		undefined ->
			Response = simple_bridge:make_response(ResponseMod, {Req, undefined}),
			Response1 = (Response:status_code(404)):data(["No application configured at this URL"]),
			Response1:build_response();
		App ->
			BaseURL = boss_web:base_url(App),
			DocRoot = boss_files:static_path(App),
			StaticPrefix = boss_web:static_prefix(App),
			Url = lists:nthtail(length(BaseURL), FullUrl),
			Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
			case lists:member(Url, boss_env:get_env(App, static_files, ["/favicon.ico", "/apple-touch-icon.png", "/robots.txt"])) of
				true ->
					(Response:file(Url)):build_response();
				false ->
					case string:substr(Url, 1, length(StaticPrefix)) of
						StaticPrefix ->
							[$/|File] = lists:nthtail(length(StaticPrefix), Url),
							Response2 = case boss_env:boss_env() of
								development ->
									Response:header("Cache-Control", "no-cache");
								_ ->
									Response
							end,
							Response3 = Response2:file([$/|File]),
							Response3:build_response();
						_ ->
							build_dynamic_response(App, Request, Response, Url)
					end
			end
	end.

generate_session_id(Request) ->
    case boss_env:get_env(session_enable, true) of
        true ->
            SessionKey = boss_session:get_session_key(),
            boss_session:new_session(Request:cookie(SessionKey));
        false ->
            undefined
    end.

build_dynamic_response(App, Request, Response, Url) ->
    Mode = case boss_env:is_developing_app(App) of
        true -> development;
        false -> production
    end,
    AppInfo = boss_web:application_info(App),
    TranslatorPid = boss_web:translator_pid(App),
    RouterPid = boss_web:router_pid(App),
    ControllerList = boss_files:web_controller_list(App),
    {Time, {StatusCode, Headers, Payload}} = timer:tc(?MODULE, process_request, [
            AppInfo#boss_app_info{
                translator_pid = TranslatorPid,
                router_pid = RouterPid,
                controller_modules = ControllerList
            },
            Request, Mode, Url]),
    ErrorFormat = "~s ~s [~p] ~p ~pms",
    RequestMethod = Request:request_method(),
    FullUrl = Request:path(),
    ErrorArgs = [RequestMethod, FullUrl, App, StatusCode, Time div 1000],
    case StatusCode of
        500 -> error_logger:error_msg(ErrorFormat, ErrorArgs);
        404 -> error_logger:warning_msg(ErrorFormat, ErrorArgs);
        _ -> error_logger:info_msg(ErrorFormat, ErrorArgs)
    end,
    Response1 = (Response:status_code(StatusCode)):data(Payload),
    Response2 = lists:foldl(fun({K, V}, Acc) -> Acc:header(K, V) end, Response1, Headers),
    case Payload of
        {stream, Generator, Acc0} ->
            TransferEncoding = case Request:protocol_version() of {1, 1} -> chunked; _ -> identity end, 
            Response3 = Response2:data(chunked),
            Response3:build_response(),
            process_stream_generator(Request, TransferEncoding, RequestMethod, Generator, Acc0);
        _ ->
            (Response2:data(Payload)):build_response()
    end.

process_request(#boss_app_info{ doc_prefix = DocPrefix } = AppInfo, Req, development, DocPrefix) ->
    {Result, SessionID1} = case catch load_and_execute(development, {"doc", [], []}, AppInfo, [{request, Req}]) of
        {'EXIT', Reason} ->
            {{error, Reason}, generate_session_id(Req)};
        {R, S} ->
            {R, S}
    end,
    process_result_and_add_session(AppInfo, [{request, Req}, {session_id, SessionID1}], Result);
process_request(AppInfo, Req, development, Url) ->
    DocPrefixPlusSlash = AppInfo#boss_app_info.doc_prefix ++ "/",
    {Result, SessionID1} = case string:substr(Url, 1, length(DocPrefixPlusSlash)) of
        DocPrefixPlusSlash ->
            ModelName = lists:nthtail(length(DocPrefixPlusSlash), Url),
            case string:chr(ModelName, $.) of
                0 ->
                    case catch load_and_execute(development, {"doc", ModelName, []}, AppInfo, [{request, Req}]) of
                        {'EXIT', Reason} ->
                            {{error, Reason}, undefined};
                        {R, S} ->
                            {R, S}
                    end;
                _ ->
                    {{not_found, "File not found"}, undefined}
            end;
        _ ->
            ControllerList = boss_files:web_controller_list(AppInfo#boss_app_info.application),
            RouterPid = AppInfo#boss_app_info.router_pid,
            boss_router:set_controllers(RouterPid, ControllerList),
            boss_router:reload(RouterPid),
            process_dynamic_request(AppInfo, Req, development, Url)
    end,
    process_result_and_add_session(AppInfo, [{request, Req}, {session_id, SessionID1}], Result);
process_request(AppInfo, Req, Mode, Url) ->
    {Result, SessionID1} = process_dynamic_request(AppInfo, Req, Mode, Url),
    process_result_and_add_session(AppInfo, [{request, Req}, {session_id, SessionID1}], Result).

process_dynamic_request(#boss_app_info{ router_pid = RouterPid } = AppInfo, Req, Mode, Url) ->
    {Result, SessionID1} = case boss_router:route(RouterPid, Url) of
        {ok, {Application, Controller, Action, Tokens}} when Application =:= AppInfo#boss_app_info.application ->
            Location = {Controller, Action, Tokens},
            case catch load_and_execute(Mode, Location, AppInfo, [{request, Req}]) of
                {'EXIT', Reason} ->
                    {{error, Reason}, undefined};
                {{not_found, Message}, S1} ->
                    {process_not_found(Message, AppInfo, [{request, Req}, {session_id, S1}], Mode), S1};
                {not_found, S1} ->
                    {process_not_found("File not found.", AppInfo, [{request, Req}, {session_id, S1}], Mode), S1};
                Ok ->
                    Ok
            end;
        {ok, {OtherApplication, Controller, Action, Tokens}} ->
            {{redirect, {OtherApplication, Controller, Action, Tokens}}, undefined};
        not_found ->
            {process_not_found("No routes matched the requested URL.", AppInfo, [{request, Req}], Mode),
                undefined}
    end,
    FinalResult = case Result of
        {error, Payload} ->
            process_error(Payload, AppInfo, [{request, Req}, {session_id, SessionID1}], Mode);
        _ ->
            Result
    end,
    {FinalResult, SessionID1}.

process_not_found(Message, #boss_app_info{ router_pid = RouterPid } = AppInfo, RequestContext, Mode) ->
    case boss_router:handle(RouterPid, 404) of
        {ok, {Application, Controller, Action, Tokens}} when Application =:= AppInfo#boss_app_info.application ->
            Location = {Controller, Action, Tokens},
            case catch load_and_execute(Mode, Location, AppInfo, RequestContext) of
                {'EXIT', Reason} ->
                    {error, Reason};
                {{ok, Payload, Headers}, _} ->
                    {not_found, Payload, Headers}
            end;
        {ok, OtherLocation} ->
            {redirect, OtherLocation};
        not_found ->
            {not_found, [Message, " ",
                    "Additionally, no handler was found for processing 404 errors. ",
                    "You probably want to modify ", boss_files:routes_file(AppInfo#boss_app_info.application), " to prevent errors like this one."]}
    end.

process_error(Payload, AppInfo, RequestContext, development) ->
    error_logger:error_report(Payload),
    ExtraMessage = case boss_router:handle(AppInfo#boss_app_info.router_pid, 500) of
        not_found ->
            ["This message will appear in production; you may want to define a 500 handler in ", boss_files:routes_file(AppInfo#boss_app_info.application)];
        _Route ->
            "(Don't worry, this message will not appear in production.)"
    end,
    render_error(io_lib:print(Payload), ExtraMessage, AppInfo, RequestContext);
    
process_error(Payload, #boss_app_info{ router_pid = RouterPid } = AppInfo, RequestContext, Mode) ->
    error_logger:error_report(Payload),
    case boss_router:handle(RouterPid, 500) of
        {ok, {Application, Controller, Action, Tokens}} when Application =:= AppInfo#boss_app_info.application ->
            ErrorLocation = {Controller, Action, Tokens},
            case catch load_and_execute(Mode, ErrorLocation, AppInfo, RequestContext) of
                {'EXIT', Reason} ->
                    {error, ["Error in 500 handler: <pre>", io_lib:print(Reason), "</pre>"], []};
                {Result, _Session} ->
                    Result
            end;
        {ok, OtherLocation} ->
            {redirect, OtherLocation};
        not_found ->
            render_error(io_lib:print(Payload), [], AppInfo, RequestContext)
    end.

process_stream_generator(_Req, _TransferEncoding, 'HEAD', _Generator, _Acc) ->
    ok;
process_stream_generator(Req, chunked, Method, Generator, Acc) ->
    case Generator(Acc) of
        {output, Data, Acc1} ->
            case iolist_size(Data) of
                0 -> ok;
                Length ->
                    Chunk = [io_lib:format("~.16b\r\n", [Length]), Data, <<"\r\n">>],
                    ok = mochiweb_socket:send(Req:socket(), Chunk)
            end,
            process_stream_generator(Req, chunked, Method, Generator, Acc1);
        done -> ok = mochiweb_socket:send(Req:socket(), ["0\r\n\r\n"])
    end;
process_stream_generator(Req, identity, Method, Generator, Acc) ->
    case Generator(Acc) of
        {output, Data, Acc1} ->
            mochiweb_socket:send(Req:socket(), Data),
            process_stream_generator(Req, identity, Method, Generator, Acc1);
        done -> ok
    end.

process_result_and_add_session(AppInfo, RequestContext, Result) ->
    Req = proplists:get_value(request, RequestContext),
    {StatusCode, Headers, Payload} = process_result(AppInfo, Req, Result),
    Headers1 = case proplists:get_value(session_id, RequestContext) of
        undefined ->
            Headers;
        SessionID ->
            SessionExpTime = boss_session:get_session_exp_time(),
            CookieOptions = [{path, "/"}, {max_age, SessionExpTime}],
            CookieOptions2 = case boss_env:get_env(session_domain, undefined) of
                undefined ->
                    CookieOptions;
                CookieDomain ->
                    lists:merge(CookieOptions, [{domain, CookieDomain}])
            end,
            HttpOnly = boss_env:get_env(session_cookie_http_only, false),
            Secure = boss_env:get_env(session_cookie_secure, false),
            CookieOptions3 = lists:merge (CookieOptions2, [{http_only, HttpOnly},
                                                           {secure, Secure}]),
            SessionKey = boss_session:get_session_key(),
            lists:merge(Headers, [ mochiweb_cookies:cookie(SessionKey, SessionID, CookieOptions3) ])
    end,
    {StatusCode, Headers1, Payload}.

process_result(AppInfo, Req, {Status, Payload}) ->
    process_result(AppInfo, Req, {Status, Payload, []});
process_result(_, _, {ok, Payload, Headers}) ->
    {200, merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(AppInfo, Req, {stream, Generator, Acc0}) ->
    process_result(AppInfo, Req, {stream, Generator, Acc0, []});
process_result(_, _, {stream, Generator, Acc0, Headers}) ->
    {200, merge_headers(Headers, [{"Content-Type", "text/html"}]), {stream, Generator, Acc0}};
process_result(AppInfo, Req, {moved, "http://"++Where, Headers}) ->
    process_result(AppInfo, Req, {moved_external, "http://"++Where, Headers});
process_result(AppInfo, Req, {moved, "https://"++Where, Headers}) ->
    process_result(AppInfo, Req, {moved_external, "https://"++Where, Headers});
process_result(AppInfo, Req, {moved, {Application, Controller, Action, Params}, Headers}) ->
    RouterPid = if
        AppInfo#boss_app_info.application =:= Application ->
            AppInfo#boss_app_info.router_pid;
        true ->
            boss_web:router_pid(Application)
    end,
    ExtraParams = [{application, Application}, {controller, Controller}, {action, Action}],
    URL = boss_erlydtl_tags:url(ExtraParams ++ Params, [
            {host, Req:header(host)},
            {base_url, AppInfo#boss_app_info.base_url},
            {application, AppInfo#boss_app_info.application},
            {router_pid, RouterPid}]),
    {301, [{"Location", URL}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(AppInfo, _, {moved, Where, Headers}) ->
    {301, [{"Location", AppInfo#boss_app_info.base_url ++ Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(_, _, {moved_external, Where, Headers}) ->
    {301, [{"Location", Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(AppInfo, Req, {redirect, "http://"++Where, Headers}) ->
    process_result(AppInfo, Req, {redirect_external, "http://"++Where, Headers});
process_result(AppInfo, Req, {redirect, "https://"++Where, Headers}) ->
    process_result(AppInfo, Req, {redirect_external, "https://"++Where, Headers});
process_result(AppInfo, Req, {redirect, {Application, Controller, Action, Params}, Headers}) ->
    RouterPid = if
        AppInfo#boss_app_info.application =:= Application ->
            AppInfo#boss_app_info.router_pid;
        true ->
            boss_web:router_pid(Application)
    end,
    ExtraParams = [{application, Application}, {controller, Controller}, {action, Action}],
    URL = boss_erlydtl_tags:url(ExtraParams ++ Params, [
            {host, Req:header(host)},
            {base_url, AppInfo#boss_app_info.base_url},
            {application, AppInfo#boss_app_info.application},
            {router_pid, RouterPid}]),
    {302, [{"Location", URL}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(AppInfo, _, {redirect, Where, Headers}) ->
    {302, [{"Location", AppInfo#boss_app_info.base_url ++ Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(_, _, {redirect_external, Where, Headers}) ->
    {302, [{"Location", Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(_, _, {unauthorized, Payload, Headers}) ->
    {401, merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(_, _, {not_found, Payload, Headers}) ->
    {404, merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(_, _, {error, Payload, Headers}) ->
    {500, merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(_, _, {StatusCode, Payload, Headers}) when is_integer(StatusCode) ->
    {StatusCode, merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload}.

load_and_execute(Mode, {Controller, _, _} = Location, AppInfo, RequestContext) when Mode =:= production; Mode =:= testing->
    case boss_files:is_controller_present(AppInfo#boss_app_info.application, Controller,
            AppInfo#boss_app_info.controller_modules) of
        true -> execute_action(Location, AppInfo, RequestContext);
        false -> {render_view(Location, AppInfo, RequestContext)}
    end;
%% @desc handle requests to /doc and return EDoc generated html
load_and_execute(development, {"doc", ModelName, _}, AppInfo, RequestContext) ->
    Result = case boss_load:load_models(AppInfo#boss_app_info.application) of
        {ok, ModelModules} ->
            % check if ModelName is in list of models
            case lists:member(ModelName, lists:map(fun atom_to_list/1, ModelModules)) of
                true ->
                    Model = list_to_atom(ModelName),
                    {Model, Edoc} = boss_model_manager:edoc_module(
                        boss_files:model_path(ModelName++".erl"), [{private, true}]),
                    {ok, correct_edoc_html(Edoc, AppInfo), []};
                false ->
                    % ok, it's not model, so it could be web controller
                    {ok, Controllers} = boss_load:load_web_controllers(AppInfo#boss_app_info.application),
                    case lists:member(ModelName, lists:map(fun atom_to_list/1, Controllers)) of
                            true ->
                                Controller = list_to_atom(ModelName),  
                                {Controller, Edoc} = edoc:get_doc(boss_files:web_controller_path(ModelName++".erl"), [{private, true}]),
                                {ok, correct_edoc_html(Edoc, AppInfo), []};
                            false ->
                                % nope, so just render index page
                                case boss_html_doc_template:render([
                                            {application, AppInfo#boss_app_info.application},
                                            {'_doc', AppInfo#boss_app_info.doc_prefix},
                                            {'_static', AppInfo#boss_app_info.static_prefix},
                                            {'_base_url', AppInfo#boss_app_info.base_url},
                                            {models, ModelModules},
                                            {controllers, Controllers}]) of
                                    {ok, Payload} ->
                                        {ok, Payload, []};
                                    Err ->
                                        Err
                                end
                    end
            end;
        {error, ErrorList} ->
            render_errors(ErrorList, AppInfo, RequestContext)
    end,
    {Result, proplists:get_value(session_id, RequestContext)};
load_and_execute(development, {Controller, _, _} = Location, AppInfo, RequestContext) ->
    SessionID = proplists:get_value(session_id, RequestContext),
    Application = AppInfo#boss_app_info.application,
    Res1 = boss_load:load_mail_controllers(Application),
    Res2 = case Res1 of
        {ok, _} -> boss_load:load_libraries(Application);
        _ -> Res1
    end,
    Res3 = case Res2 of
        {ok, _} -> boss_load:load_view_lib_modules(Application);
        _ -> Res2
    end,
    Res4 = case Res3 of
        {ok, _} -> boss_load:load_services_websockets(Application);
        _ -> Res3
    end,
    Res5 = case Res4 of
        {ok, _} -> boss_load:load_web_controllers(Application);
        _ -> Res4
    end,
    Res6 = case Res5 of
        {ok, Controllers} ->
            case boss_files:is_controller_present(Application, Controller,
                    lists:map(fun atom_to_list/1, Controllers)) of
                true ->
                    case boss_load:load_models(Application) of
                        {ok, _} ->
                            execute_action(Location, AppInfo, RequestContext);
                        {error, ErrorList} ->
                            {render_errors(ErrorList, AppInfo, RequestContext), SessionID}
                    end;
                false ->
                    {render_view(Location, AppInfo, RequestContext), SessionID}
            end;
        {error, ErrorList} ->
            {render_errors(ErrorList, AppInfo, RequestContext), SessionID}
    end,
    Res6.

%% @desc function to correct path errors in HTML output produced by Edoc
correct_edoc_html(Edoc, AppInfo) ->
    Result = edoc:layout(Edoc, [{stylesheet, AppInfo#boss_app_info.base_url++AppInfo#boss_app_info.static_prefix++"/edoc/stylesheet.css"}]),
    Result2 = re:replace(Result, "overview-summary.html", "./", [{return,list}, global]),
    Result3 = re:replace(Result2, "erlang.png", AppInfo#boss_app_info.base_url++AppInfo#boss_app_info.static_prefix++"/edoc/erlang.png", [{return,list}, global]),
    Result3.

%% @desc generates HTML output for errors. called from process_error/5
%% (seems to be called on runtime error)
render_error(Error, ExtraMessage, AppInfo, RequestContext) ->
    case boss_html_error_template:render(RequestContext ++ [
                {error, Error}, {extra_message, ExtraMessage}, {appinfo, AppInfo},
                {'_base_url', AppInfo#boss_app_info.base_url},
                {'_static', AppInfo#boss_app_info.static_prefix}]) of
        {ok, Payload} ->
            {error, Payload, []};
        Err ->
            Err
    end.

%% @desc generates HTML output for errors. called from load_and_execute/5
%% (seems to be called on parse error)
render_errors(ErrorList, AppInfo, RequestContext) ->
    case boss_html_errors_template:render(RequestContext ++ [
                {errors, ErrorList}, 
                {'_base_url', AppInfo#boss_app_info.base_url},
                {'_static', AppInfo#boss_app_info.static_prefix}]) of
        {ok, Payload} ->
            {ok, Payload, []};
        Err ->
            Err
    end.

execute_action(Location, AppInfo, RequestContext) ->
    execute_action(Location, AppInfo, RequestContext, []).

execute_action({Controller, Action}, AppInfo, RequestContext, LocationTrail) ->
    execute_action({Controller, Action, []}, AppInfo, RequestContext, LocationTrail);
execute_action({Controller, Action, Tokens}, AppInfo, RequestContext, LocationTrail) when is_atom(Action) ->
    execute_action({Controller, atom_to_list(Action), Tokens}, AppInfo, RequestContext, LocationTrail);
execute_action({Controller, Action, Tokens} = Location, AppInfo, RequestContext, LocationTrail) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    case lists:member(Location, LocationTrail) of
        true ->
            {{error, "Circular redirect!"}, SessionID};
        false ->
            % do not convert a list to an atom until we are sure the controller/action
            % pair exists. this prevents a memory leak due to atom creation.
            Adapters = [boss_controller_adapter_pmod, 
                        boss_controller_adapter_elixir],

            Adapter = lists:foldl(fun
                    (A, false) ->
                        case A:accept(AppInfo#boss_app_info.application, Controller,
                                AppInfo#boss_app_info.controller_modules) of
                            true -> A;
                            _ -> false
                        end;
                    (_, Acc) -> Acc
                end, false, Adapters),

            SessionID1 = case SessionID of
                undefined ->
                    case Adapter:wants_session(AppInfo#boss_app_info.application, Controller,
                            AppInfo#boss_app_info.controller_modules) of
                        true -> generate_session_id(Req);
                        _ -> SessionID
                    end;
                _ -> SessionID
            end,

            RequestMethod = Req:request_method(),

            RequestContext1 = [{request, Req}, {session_id, SessionID1}, {method, RequestMethod}, 
                {action, Action}, {tokens, Tokens}],

            AdapterInfo = Adapter:init(AppInfo#boss_app_info.application, Controller,
                AppInfo#boss_app_info.controller_modules, RequestContext1),

            RequestContext2 = [{controller_module, element(1, AdapterInfo)}|RequestContext1],

            {ActionResult, RequestContext3} = case apply_before_filters(Adapter, AdapterInfo, RequestContext2) of 
                {ok, RC3} ->
                    EffectiveRequestMethod = case Req:request_method() of
                        'HEAD' -> 'GET';
                        Method -> Method
                    end,

                    {Adapter:action(AdapterInfo, lists:keyreplace(method, 1, RC3, {method, EffectiveRequestMethod})), RC3};
                {not_ok, RC3, NotOK} ->
                    {NotOK, RC3}
            end,

            RenderedResult = case ActionResult of
                undefined ->
                    render_view(Location, AppInfo, RequestContext3, [], []);
                _ ->
                    ExpandedResult = expand_action_result(ActionResult),
                    TransformedResult = apply_middle_filters(Adapter, AdapterInfo, 
                        RequestContext, ExpandedResult),
                    process_action_result({Location, RequestContext3, [Location|LocationTrail]},
                        TransformedResult, [], AppInfo)
            end,

            FinalResult = apply_after_filters(Adapter, AdapterInfo, RequestContext3, RenderedResult),

            {FinalResult, SessionID1}
    end.

process_before_filter_result({ok, Context}, _) -> {ok, Context};
process_before_filter_result(NotOK, Context) -> {not_ok, Context, NotOK}.

filter_config(Filter) ->
    FilterKey = case proplists:get_value('config_key', Filter:module_info(exports)) of
        0 -> Filter:config_key();
        _ -> Filter
    end,
    FilterValue = case proplists:get_value(FilterKey, boss_env:get_env(controller_filter_config, [])) of
        undefined ->
            case proplists:get_value('config_default_value', Filter:module_info(exports)) of
                0 -> Filter:config_default_value();
                _ -> undefined
            end;
        Value -> Value
    end,
    {FilterKey, FilterValue}.

apply_before_filters(Adapter, AdapterInfo, RequestContext) ->
    % legacy API
    case Adapter:before_filter(AdapterInfo, RequestContext) of
        {ok, RequestContext1} ->
            % new API
            GlobalFilters = filters_for_function('before_filter'),
            ActionFilters = Adapter:filters('before', AdapterInfo, RequestContext, GlobalFilters),
            lists:foldl(fun
                    (Filter, {ok, Context}) when is_atom(Filter) ->
                        {FilterKey, DefaultConfig} = filter_config(Filter),
                        FilterConfig = Adapter:filter_config(AdapterInfo, FilterKey, DefaultConfig, RequestContext),
                        FilterResult = case proplists:get_value(before_filter, Filter:module_info(exports)) of
                            2 -> Filter:before_filter(FilterConfig, Context);
                            _ -> {ok, Context}
                        end,
                        process_before_filter_result(FilterResult, Context);
                    (_Filter, NotOK) -> NotOK
                end, {ok, RequestContext1}, ActionFilters);
        Other ->
            {not_ok, RequestContext, Other}
    end.

apply_middle_filters(Adapter, AdapterInfo, RequestContext, ActionResult) ->
    GlobalFilters = filters_for_function('middle_filter'),
    ActionFilters = Adapter:filters('middle', AdapterInfo, RequestContext, GlobalFilters),

    lists:foldl(fun
            (_Filter, {StatusCode, Payload, Headers}) when is_integer(StatusCode) ->
                {StatusCode, Payload, Headers};
            (_Filter, {ok, Payload, Headers}) ->
                {ok, Payload, Headers};
            (Filter, Result) when is_atom(Filter) ->
                {FilterKey, DefaultConfig} = filter_config(Filter),
                FilterConfig = Adapter:filter_config(AdapterInfo, FilterKey, DefaultConfig, RequestContext),
                case proplists:get_value(middle_filter, Filter:module_info(exports)) of
                    3 -> Filter:middle_filter(Result, FilterConfig, RequestContext);
                    _ -> Result
                end
        end, ActionResult, ActionFilters).

apply_after_filters(Adapter, AdapterInfo, RequestContext, RenderedResult) ->
    GlobalFilters = filters_for_function('after_filter'),
    ActionFilters = Adapter:filters('after', AdapterInfo, RequestContext, GlobalFilters),

    % legacy API
    RenderedResult1 = Adapter:after_filter(AdapterInfo, RequestContext, RenderedResult),

    % new API
    lists:foldl(fun
            (Filter, Rendered) when is_atom(Filter) ->
                {FilterKey, DefaultConfig} = filter_config(Filter),
                FilterConfig = Adapter:filter_config(AdapterInfo, FilterKey, DefaultConfig, RequestContext),
                case proplists:get_value(after_filter, Filter:module_info(exports)) of
                    3 -> Filter:after_filter(Rendered, FilterConfig, RequestContext);
                    _ -> Rendered
                end
        end, RenderedResult1, ActionFilters).

handle_news_for_cache(_, _, {Prefix, Key}) ->
    boss_cache:delete(Prefix, Key),
    {ok, cancel_watch}.

process_location(Controller,  [{_, _}|_] = Where, AppInfo) ->
    {_, TheController, TheAction, CleanParams} = process_redirect(Controller, Where, AppInfo),
    ControllerModule = list_to_atom(boss_files:web_controller(
            AppInfo#boss_app_info.application, Controller, AppInfo#boss_app_info.controller_modules)),
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

expand_action_result(Keyword) when Keyword =:= ok; Keyword =:= render ->
    {render, [], []};
expand_action_result({Keyword, Data}) when Keyword =:= ok; Keyword =:= render ->
    {render, Data, []};
expand_action_result({ok, Data, Headers}) ->
    {render, Data, Headers};
expand_action_result({render_other, OtherLocation}) ->
    {render_other, OtherLocation, [], []};
expand_action_result({render_other, OtherLocation, Data}) ->
    {render_other, OtherLocation, Data, []};
expand_action_result({redirect, Where}) ->
    {redirect, Where, []};
expand_action_result({js, Data}) ->
    {js, Data, []};
expand_action_result({json, Data}) ->
    {json, Data, []};
expand_action_result({jsonp, Callback, Data}) ->
    {jsonp, Callback, Data, []};
expand_action_result({output, Payload}) ->
    {output, Payload, []};
expand_action_result(Other) ->
    Other.

process_action_result({Location, RequestContext, _}, {render, Data, Headers}, ExtraHeaders, AppInfo) ->
    render_view(Location, AppInfo, RequestContext, Data, merge_headers(Headers, ExtraHeaders));

process_action_result({{Controller, _, _}, RequestContext, _}, {render_other, OtherLocation, Data, Headers}, ExtraHeaders, AppInfo) ->
    TheApplication = proplists:get_value(application, OtherLocation, AppInfo#boss_app_info.application),
    TheAppInfo = boss_web:application_info(TheApplication),
    TheAppInfo1 = TheAppInfo#boss_app_info{ translator_pid = boss_web:translator_pid(TheApplication),
                                            router_pid = boss_web:router_pid(TheApplication) },
    render_view(process_location(Controller, OtherLocation, TheAppInfo1),
        TheAppInfo1, RequestContext, Data, merge_headers(Headers, ExtraHeaders));

process_action_result({{Controller, _, _}, RequestContext, LocationTrail}, {action_other, OtherLocation}, _, AppInfo) ->
    execute_action(process_location(Controller, OtherLocation, AppInfo), AppInfo, RequestContext, LocationTrail);

process_action_result({_, RequestContext, LocationTrail}, not_found, _, AppInfo) ->
    case boss_router:handle(AppInfo#boss_app_info.router_pid, 404) of
        {ok, {Application, Controller, Action, Params}} when Application =:= AppInfo#boss_app_info.application ->
            case execute_action({Controller, Action, Params}, AppInfo, RequestContext, LocationTrail) of
                {ok, Payload, Headers} ->
                    {not_found, Payload, Headers};
                Other ->
                    Other
            end;
        {ok, {OtherApplication, Controller, Action, Params}} ->
            {redirect, {OtherApplication, Controller, Action, Params}};
        not_found ->
            {not_found, "The requested page was not found. Additionally, no handler was found for processing 404 errors."}
    end;

process_action_result({{Controller, _, _}, _, _}, {redirect, Where, Headers}, ExtraHeaders, AppInfo) ->
    {redirect, process_redirect(Controller, Where, AppInfo), merge_headers(Headers, ExtraHeaders)};

process_action_result(Info, {js, Data, Headers}, ExtraHeaders, AppInfo) ->
    process_action_result(Info, {output, Data, merge_headers(Headers, [{"Content-Type", "application/javascript"}])},
        ExtraHeaders, AppInfo);

process_action_result(Info, {json, Data, Headers}, ExtraHeaders, AppInfo) ->
    process_action_result(Info, {output, boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
            merge_headers(Headers, [{"Content-Type", "application/json"}])}, ExtraHeaders, AppInfo);

process_action_result(Info, {jsonp, Callback, Data, Headers}, ExtraHeaders, AppInfo) ->
    JsonData  = boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
    process_action_result(Info, {output, Callback ++ "(" ++ JsonData ++ ");",
            merge_headers(Headers, [{"Content-Type", "application/javascript"}])}, ExtraHeaders, AppInfo);

process_action_result(_, {output, Payload, Headers}, ExtraHeaders, _) ->
    {ok, Payload, merge_headers(Headers, ExtraHeaders)};

process_action_result(_, Else, _, _) ->
    Else.

render_view(Location, AppInfo, RequestContext) ->
    render_view(Location, AppInfo, RequestContext, []).

render_view(Location, AppInfo, RequestContext, Variables) ->
    render_view(Location, AppInfo, RequestContext, Variables, []).

render_view({Controller, Template, _}, AppInfo, RequestContext, Variables, Headers) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    TryExtensions = boss_files:template_extensions(),
    LoadResult = lists:foldl(fun
            (Ext, {error, not_found}) ->
                ViewPath = boss_files:web_view_path(Controller, Template, Ext),
                boss_load:load_view_if_dev(AppInfo#boss_app_info.application,
                    ViewPath, AppInfo#boss_app_info.view_modules,
                    AppInfo#boss_app_info.translator_pid);
            (_, Acc) ->
                Acc
        end, {error, not_found}, TryExtensions),
    BossFlash = boss_flash:get_and_clear(SessionID),
    SessionData = boss_session:get_session_data(SessionID),
    case LoadResult of
        {ok, Module, TemplateAdapter} ->
            TranslatableStrings = TemplateAdapter:translatable_strings(Module),
            {Lang, TranslationFun} = choose_translation_fun(AppInfo#boss_app_info.translator_pid,
                TranslatableStrings, Req:header(accept_language),
                proplists:get_value(language, RequestContext)),
            BeforeVars = case proplists:get_value('_before', RequestContext) of
                undefined -> [];
                AuthInfo -> [{"_before", AuthInfo}]
            end,
            RenderVars = BossFlash ++ BeforeVars ++ [{"_lang", Lang}, {"_session", SessionData},
                            {"_req", Req}, {"_base_url", AppInfo#boss_app_info.base_url}|Variables],
            case TemplateAdapter:render(Module, [{"_vars", RenderVars}|RenderVars],
                    [{translation_fun, TranslationFun}, {locale, Lang},
                        {host, Req:header(host)}, {application, atom_to_list(AppInfo#boss_app_info.application)},
                        {controller, Controller}, {action, Template},
                        {router_pid, AppInfo#boss_app_info.router_pid}]) of
                {ok, Payload} ->
                    {ok, Payload, merge_headers([{"Content-Language", Lang}], Headers)};
                Err ->
                    Err
            end;
        {error, not_found} ->
            AnyViewPath = boss_files:web_view_path(Controller, Template, "{" ++ string:join(TryExtensions, ",") ++ "}"),
            {not_found, io_lib:format("The requested template (~p) was not found.", [AnyViewPath]) };
        {error, {File, [{0, _Module, "Failed to read file"}]}} ->
            {not_found, io_lib:format("The requested template (~p) was not found.", [File]) };
        {error, Error}->
            render_errors([Error], AppInfo, RequestContext)
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
                case TranslationScore > BestTranslationScore andalso TranslationCoverage > 0.0 of
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

% merges headers with preference on Headers1.
merge_headers(Headers1, Headers2) ->
    simple_bridge_util:ensure_headers(Headers1, Headers2).

filters_for_function(Function) ->
    lists:foldr(fun(Module, List) ->
                Exports = Module:module_info(exports),
                case proplists:get_value(Function, Exports) of
                    undefined -> List;
                    _ -> [Module|List]
                end
        end, [], ?BUILTIN_CONTROLLER_FILTERS ++ boss_env:get_env(controller_filter_modules, [])).
