-module(boss_web_controller).
-behaviour(gen_server).

-export([start_link/0, start_link/1, process_request/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([merge_headers/2]).
-export([handle_news_for_cache/3]).

-export([run_init_scripts/1]).

-export([execute_action/4, filter_config/1, filters_for_function/1
                      ]).


-include("boss_web.hrl").

start_link() ->
    init_master_node().

init_master_node() -> start_link([]).

start_link(Config) ->
    gen_server:start_link({local, boss_web}, ?MODULE, Config, []).


terminate(Reason, #state{ is_master_node = true } = State) ->
    boss_news:stop(),
    boss_mq:stop(),
    boss_session:stop(),
    stop_smtp(),
    terminate(Reason, State#state{ is_master_node = false });

terminate(_Reason, State) ->
    lists:map(fun(AppInfo) ->
                stop_init_scripts(AppInfo#boss_app_info.application, AppInfo#boss_app_info.init_data)
        end, State#state.applications),
    Services = [error_logger, boss_translator, boss_router, boss_model_manager, boss_cache, mochiweb_http],
    [Service:stop() ||Service <- Services],
    application:stop(elixir).


terminate_smtp(Pid) when is_pid(Pid) ->
    gen_smtp_server:stop(Pid);
terminate_smtp(_) ->
    ok.


stop_smtp() ->
    case boss_env:get_env(smtp_server_enable, false) of
        true ->
            SMTPPid = global:whereis_name(boss_smtp_server),
	    terminate_smtp(SMTPPid);
        _ ->
            ok
    end.

init_web_server_options() ->
    {ServerMod, RequestMod, ResponseMod} = case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
        mochiweb -> {mochiweb_http, mochiweb_request_bridge, mochiweb_response_bridge};
					       cowboy   -> {cowboy, mochiweb_request_bridge, mochiweb_response_bridge}
                                           end,
    {RequestMod, ResponseMod, ServerMod}.



init(Config) ->
    ThisNode					= erlang:node(),
    Env						= boss_web_controller_init:init_services(),
   
    {ok,MasterNode}				= boss_web_controller_init:init_master_node(Env, ThisNode),
    boss_web_controller_init:init_mail_service(),

    {RequestMod, ResponseMod, ServerMod}	= init_web_server_options(),
    {SSLEnable, SSLOptions}			= boss_web_controller_init:init_ssl(),

    ServicesSupPid				= boss_web_controller_init:init_master_services(ThisNode, MasterNode),

    ServerConfig                                = init_server_config(Config,
								     RequestMod,
							             ResponseMod),
    Pid						= boss_web_controller_init:init_webserver(ThisNode, MasterNode, ServerMod, SSLEnable,
											  SSLOptions, ServicesSupPid, ServerConfig),
    {ok, #state{ service_sup_pid = ServicesSupPid, http_pid = Pid, is_master_node = (ThisNode =:= MasterNode) }, 0}.

init_server_config(Config, RequestMod, ResponseMod) ->
    [{loop, fun(Req) ->
		    boss_web_controller_handle_request:handle_request(Req, RequestMod, ResponseMod)
            end} | Config].




handle_info(timeout, State = #state{service_sup_pid = ServicesSupPid}) ->
    Applications	= boss_env:get_env(applications, []),
    AppInfoList         = start_boss_applications(Applications,
						  ServicesSupPid),

    case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
        cowboy ->
            boss_web_controller_cowboy:dispatch_cowboy(Applications);
        _Oops ->
            _Oops
    end,
    {noreply, State#state{ applications = AppInfoList }}.

start_boss_applications( Applications, ServicesSupPid) ->
    lists:map(fun
	   (AppName) ->
	       application:start(AppName),
	       {TranslatorSupPid, BaseURL, IsMasterNode, StaticPrefix,
		DocPrefix, DomainList, ModelList, ViewList, ControllerList,
                RouterSupPid}  = boss_web_controller_util:unpack_application_env(AppName),
					    
	       init_app_load_on_dev(AppName,
				    TranslatorSupPid),
					    
	       enable_master_apps(ServicesSupPid, AppName, BaseURL,
				  IsMasterNode),
					    
	       boss_web_controller_util:make_boss_app_info(AppName, BaseURL, StaticPrefix, DocPrefix,
				                           DomainList, ModelList, ViewList,
				                           ControllerList, RouterSupPid,
				                           TranslatorSupPid)
	      end, Applications).

init_app_load_on_dev(AppName, TranslatorSupPid) ->
    case boss_env:is_developing_app(AppName) of
	true -> boss_load:load_all_modules(AppName, TranslatorSupPid);
	false -> ok
    end.

enable_master_apps(ServicesSupPid, AppName, BaseURL, IsMasterNode) ->
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
    end.


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_init_scripts(AppName) ->
    lists:foldl(fun(File, Acc) ->
			CompileResult = boss_compiler:compile(File, [{include_dirs, [boss_files:include_dir() | boss_env:get_env(boss, include_dirs, [])]}]),
			process_compile_result(File, Acc, CompileResult)
		end, [], boss_files:init_file_list(AppName)).

process_compile_result(File, Acc, {ok, Module}) ->
    InitResult =  Module:init(),
    init_result(File, Acc, Module, InitResult);
process_compile_result(File, Acc, Error) ->
    error_logger:error_msg("Compilation of ~p failed: ~p~n", [File, Error]),
    Acc.

init_result(_File, Acc, Module, {ok, Info}) ->
    [{Module, Info}|Acc];
init_result(_File, Acc, Module, ok) ->
    [{Module, true}|Acc];
init_result(File, Acc, _Module,Error) ->
    error_logger:error_msg("Execution of ~p failed: ~p~n", [File, Error]),
    Acc.

generate_session_id(Request) ->
    case boss_env:get_env(session_enable, true) of
        true ->
            SessionKey = boss_session:get_session_key(),
            boss_session:new_session(Request:cookie(SessionKey));
        false ->
            undefined
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
    boss_web_controller_render:render_error(io_lib:print(Payload), ExtraMessage, AppInfo, RequestContext);
    
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
            boss_web_controller_render:render_error(io_lib:print(Payload), [], AppInfo, RequestContext)
    end.

process_result_and_add_session(AppInfo, RequestContext, Result) ->
    Req = proplists:get_value(request, RequestContext),
    {StatusCode, Headers, Payload} = process_result(AppInfo, Req, Result),
    Headers1 = case proplists:get_value(session_id, RequestContext) of
		   undefined -> Headers;
		   SessionID -> add_session_to_headers(Headers, SessionID)
               end,
    {StatusCode, Headers1, Payload}.

add_session_to_headers(Headers, SessionID) ->
    SessionExpTime	= boss_session:get_session_exp_time(),
    CookieOptions	= [{path, "/"}, {max_age, SessionExpTime}],
    CookieOptions2	= case boss_env:get_env(session_domain, undefined) of
			      undefined ->
				  CookieOptions;
			      CookieDomain ->
				  lists:merge(CookieOptions, [{domain, CookieDomain}])
			  end,
    HttpOnly		= boss_env:get_env(session_cookie_http_only, false),
    Secure		= boss_env:get_env(session_cookie_secure, false),
    CookieOptions3	= lists:merge(CookieOptions2, [{http_only, HttpOnly},
							{secure, Secure}]),
    SessionKey		= boss_session:get_session_key(),
    lists:merge(Headers, [mochiweb_cookies:cookie(SessionKey, SessionID, CookieOptions3)]).


%TODO: Refactor this
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
        false -> {boss_web_controller_render:render_view(Location, AppInfo, RequestContext)}
    end;
%% @desc handle requests to /doc and return EDoc generated html
%% Really a MONAD would be good here
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
            boss_web_controller_render:render_errors(ErrorList, AppInfo, RequestContext)
             end,
    {Result, proplists:get_value(session_id, RequestContext)};
%% Really a MONAD would be good here too
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
                            {boss_web_controller_render:render_errors(ErrorList, AppInfo, RequestContext), SessionID}
                    end;
                false ->
                    {boss_web_controller_render:render_view(Location, AppInfo, RequestContext), SessionID}
            end;
        {error, ErrorList} ->
            {boss_web_controller_render:render_errors(ErrorList, AppInfo, RequestContext), SessionID}
           end,
    Res6.

%% @desc function to correct path errors in HTML output produced by Edoc
correct_edoc_html(Edoc, AppInfo) ->
    Result = edoc:layout(Edoc, [{stylesheet, AppInfo#boss_app_info.base_url++AppInfo#boss_app_info.static_prefix++"/edoc/stylesheet.css"}]),
    Result2 = re:replace(Result, "overview-summary.html", "./", [{return,list}, global]),
    Result3 = re:replace(Result2, "erlang.png", AppInfo#boss_app_info.base_url++AppInfo#boss_app_info.static_prefix++"/edoc/erlang.png", [{return,list}, global]),
    Result3.

execute_action(Location, AppInfo, RequestContext) ->
    execute_action(Location, AppInfo, RequestContext, []).

%TODO REFACTOR THIS
execute_action({Controller, Action}, AppInfo, RequestContext, LocationTrail) ->
    execute_action({Controller, Action, []}, AppInfo, RequestContext, LocationTrail);

execute_action({Controller, Action, Tokens}, AppInfo, RequestContext, LocationTrail) when is_atom(Action) ->
    execute_action({Controller, atom_to_list(Action), Tokens}, AppInfo, RequestContext, LocationTrail);

execute_action({Controller, Action, Tokens} = Location, AppInfo, RequestContext, LocationTrail) ->
    Req		= proplists:get_value(request, RequestContext),
    SessionID	= proplists:get_value(session_id, RequestContext),
    case lists:member(Location, LocationTrail) of
        true ->
            {{error, "Circular redirect!"}, SessionID};
        false ->
            execute_action_inner(Controller, Action, Tokens, Location, AppInfo,
                                 RequestContext, LocationTrail, Req, SessionID)
    end.

execute_action_inner(Controller, Action, Tokens, Location, AppInfo,
		     RequestContext, LocationTrail, Req, SessionID) ->
    % do not convert a list to an atom until we are sure the controller/action
    % pair exists. this prevents a memory leak due to atom creation.
    Adapters                            = [boss_controller_adapter_pmod,
                                           boss_controller_adapter_elixir],
    Adapter                             = make_action_adapter(Controller, AppInfo, Adapters),
    SessionID1                          = make_action_session_id(Controller, AppInfo, Req,
						                 SessionID, Adapter),
    RequestMethod                       = Req:request_method(),
    RequestContext1                     = [{request, Req},
                                           {session_id, SessionID1},
					   {method, RequestMethod},
                                           {action, Action},
                                           {tokens, Tokens}],
    AdapterInfo                         = Adapter:init(AppInfo#boss_app_info.application, Controller,
                                                       AppInfo#boss_app_info.controller_modules, RequestContext1),
	    
    RequestContext2                     = [{controller_module, element(1, AdapterInfo)}|RequestContext1],

    {ActionResult, RequestContext3}     = apply_action(Req, Adapter,
						       AdapterInfo,
						       RequestContext2),
    RenderedResult                      = boss_web_controller_render:render_result(Location, AppInfo, RequestContext,
							                           LocationTrail, Adapter, AdapterInfo,
							                           ActionResult, RequestContext3),
    FinalResult                         = apply_after_filters(Adapter, AdapterInfo, RequestContext3, RenderedResult),
    {FinalResult, SessionID1}.

apply_action(Req, Adapter, AdapterInfo, RequestContext2) ->
    case apply_before_filters(Adapter, AdapterInfo, RequestContext2) of
	{ok, RC3} ->
	    EffectiveRequestMethod = case Req:request_method() of
					 'HEAD' -> 'GET';
					 Method -> Method
				     end,
	    
	    {Adapter:action(AdapterInfo, lists:keyreplace(method, 1, RC3, {method, EffectiveRequestMethod})), RC3};
	{not_ok, RC3, NotOK} ->
	    {NotOK, RC3}
    end.

make_action_session_id(Controller, AppInfo, Req, SessionID, Adapter) ->
    case SessionID of
	undefined ->
	    case Adapter:wants_session(AppInfo#boss_app_info.application, Controller,
				       AppInfo#boss_app_info.controller_modules) of
		true -> generate_session_id(Req);
		_ -> SessionID
	    end;
	_ -> SessionID
    end.

make_action_adapter(Controller, AppInfo, Adapters) ->
    lists:foldl(fun
		    (A, false) ->
			case A:accept(AppInfo#boss_app_info.application, Controller,
				      AppInfo#boss_app_info.controller_modules) of
			    true -> A;
			    _ -> false
			end;
		    (_, Acc) -> Acc
                end, false, Adapters).

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
