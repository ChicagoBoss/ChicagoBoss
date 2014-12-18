-module(boss_web_controller).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([merge_headers/2]).
-export([handle_news_for_cache/3]).
-export([run_init_scripts/1, generate_session_id/1]).

-export([execute_action/4, filter_config/1, filters_for_function/1]).

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

terminate(_Reason, #state{router_adapter=RouterAdapter}=State) ->
    lists:map(fun(AppInfo) ->
                stop_init_scripts(AppInfo#boss_app_info.application, AppInfo#boss_app_info.init_data)
        end, State#state.applications),
    Services = [ boss_translator, RouterAdapter, boss_model_manager, boss_cache],
    [Service:stop() ||Service <- Services],
    case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
        mochiweb ->
            mochiweb_http:stop();
        cowboy   ->
            cowboy:stop_listener(boss_http_listener),
            cowboy:stop_listener(boss_https_listener)
    end,
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
    {ServerMod, RequestMod, ResponseMod} =
        case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
            mochiweb -> {mochiweb_http, mochiweb_request_bridge, mochiweb_response_bridge};
            cowboy   -> {cowboy, mochiweb_request_bridge, mochiweb_response_bridge}
        end,
    {RequestMod, ResponseMod, ServerMod}.



init(Config) ->
    ThisNode					         = erlang:node(),
    Env						             = boss_web_controller_init:init_services(),
    {ok,MasterNode}				         = boss_web_controller_init:init_master_node(Env, ThisNode),
    
    boss_web_controller_init:init_mail_service(),
    RouterAdapter                        = boss_env:router_adapter(), 

    {RequestMod, ResponseMod, ServerMod} = init_web_server_options(),
    {SSLEnable, SSLOptions}			     = boss_web_controller_init:init_ssl(),
    ServicesSupPid				         = boss_web_controller_init:init_master_services(ThisNode, MasterNode),
    ServerConfig                         = init_server_config(Config, RequestMod, ResponseMod, RouterAdapter),
    Pid						             = boss_web_controller_init:init_webserver(
                                                ThisNode, MasterNode, ServerMod, SSLEnable,
											    SSLOptions, ServicesSupPid, ServerConfig),
    {ok, #state{ 
                router_adapter  = RouterAdapter,
                service_sup_pid = ServicesSupPid, 
                http_pid        = Pid, 
                is_master_node  = (ThisNode =:= MasterNode) }, 0}.

init_server_config(Config, RequestMod, ResponseMod, RouterAdapter) ->
    [{loop, fun(Req) ->
		    boss_web_controller_handle_request:handle_request(Req, RequestMod, ResponseMod, RouterAdapter)
            end} | Config].

handle_info(timeout, #state{service_sup_pid = ServicesSupPid} = State) ->
    Applications	= boss_env:get_env(applications, []),
    AppInfoList     = boss_web_controller_util:start_boss_applications(Applications, 
                                                                       ServicesSupPid, 
                                                                       State),
    case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
        cowboy ->
            boss_web_controller_cowboy:dispatch_cowboy(Applications);
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
handle_call(reload_routes, _From, #state{router_adapter=RouterAdapter}=State) ->
    lists:map(fun(AppInfo) ->
                [{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
                RouterAdapter:reload(RouterPid)
        end, State#state.applications),
    {reply, ok, State};
handle_call(reload_init_scripts, _From, State) ->
    NewApplications = lists:map(fun(AppInfo) ->
                stop_init_scripts(AppInfo#boss_app_info.application, AppInfo#boss_app_info.init_data),
                NewInitData = run_init_scripts(AppInfo#boss_app_info.application),
                AppInfo#boss_app_info{ init_data = NewInitData }
        end, State#state.applications),
    {reply, ok, State#state{ applications = NewApplications }};
handle_call(get_all_routes, _From, #state{router_adapter=RouterAdapter}=State) ->
    Routes = lists:map(fun(AppInfo) ->
                [{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
                {AppInfo#boss_app_info.application, RouterAdapter:get_all(RouterPid)}
        end, State#state.applications),
    {reply, Routes, State};
handle_call(get_all_models, _From, State) ->
    Models = lists:foldl(fun(AppInfo, Acc) ->
                boss_files:model_list(AppInfo#boss_app_info.application) ++ Acc
        end, [], State#state.applications),
    {reply, Models, State};
handle_call(get_all_applications, _From, State) ->
    Applications = lists:map(fun(AppInfo) -> AppInfo#boss_app_info.application end,
                             State#state.applications),
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
    {_,AppInfo,_ } = handle_call({application_info, App},_From, State),
    BaseURL        = AppInfo#boss_app_info.base_url,
    {reply, BaseURL, State};
handle_call({static_prefix, App}, _From, State) ->
    {_,AppInfo,_ } = handle_call({application_info, App},_From, State),
    StaticPrefix        = AppInfo#boss_app_info.static_prefix,
    {reply, StaticPrefix, State};
handle_call({domains, App}, _From, State) ->
    {_,AppInfo,_ } = handle_call({application_info, App},_From, State),
    DomainList        = AppInfo#boss_app_info.domains,
    {reply, DomainList, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop_init_scripts(Application, InitData) ->
    lists:foldr(fun(File, _) ->
                case boss_compiler:compile(File, [{include_dirs, [boss_files_util:include_dir() | boss_env:get_env(boss, include_dirs, [])]}]) of
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
			CompileResult = boss_compiler:compile(File, [{include_dirs, [boss_files_util:include_dir() | boss_env:get_env(boss, include_dirs, [])]}]),
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


execute_action({Controller, Action}, AppInfo, RequestContext, LocationTrail) ->
    execute_action({Controller, Action, []}, AppInfo, RequestContext, LocationTrail);

execute_action({Controller, Action, Tokens}, AppInfo, RequestContext, LocationTrail) when is_atom(Action) ->
    execute_action({Controller, atom_to_list(Action), Tokens}, AppInfo, RequestContext, LocationTrail);

execute_action({Controller, Action, Tokens} = Location, AppInfo, RequestContext, LocationTrail) ->
    Req	  	   = proplists:get_value(request, RequestContext),
    SessionID	   = proplists:get_value(session_id, RequestContext),
    IsMemberOfList = lists:member(Location, LocationTrail),
    execute_action_check_for_circular_redirect(Controller, Action, Tokens,
                                               Location, AppInfo,
                                               RequestContext, LocationTrail,
                                               Req, SessionID, IsMemberOfList).

execute_action_check_for_circular_redirect(_Controller, _Action, _Tokens,
                                           _Location, _AppInfo, _RequestContext,
                                           _LocationTrail, _Req, SessionID,
                                           true) ->
    {{error, "Circular redirect!"}, SessionID};
execute_action_check_for_circular_redirect(Controller, Action, Tokens,
                                           Location, AppInfo, RequestContext,
                                           LocationTrail, Req, SessionID,
                                           false) ->
    execute_action_inner(Controller, Action, Tokens, Location, AppInfo,
			 RequestContext, LocationTrail, Req, SessionID).


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
    AdapterInfo                         = Adapter:init(AppInfo#boss_app_info.application, 
						       Controller,
                                                       AppInfo#boss_app_info.controller_modules, 
						       RequestContext1),
    
    RequestContext2                     = [{controller_module, element(1, AdapterInfo)}|RequestContext1],
    {ActionResult, RequestContext3}     = apply_action(Req, Adapter,
						       AdapterInfo,
						       RequestContext2),
    RenderedResult                      = boss_web_controller_render:render_result(Location, AppInfo, RequestContext,
							                           LocationTrail, Adapter, AdapterInfo,
							                           ActionResult, RequestContext3),
    FinalResult                         = apply_after_filters(Adapter, 
							      AdapterInfo, 
							      RequestContext3,
							      RenderedResult),
    {FinalResult, SessionID1}.

apply_action(Req, Adapter, AdapterInfo, RequestContext2) ->
    case apply_before_filters(Adapter, AdapterInfo, RequestContext2) of
	{ok, RC3} ->
	    EffectiveRequestMethod = case Req:request_method() of
					 'HEAD' -> 'GET';
					 Method -> Method
				     end,
	  
            RequestContext = lists:keyreplace(method, 1, RC3, {method, EffectiveRequestMethod}),
	    {call_controller_action(Adapter, AdapterInfo, RequestContext),
	     RC3};
	{not_ok, RC3, NotOK} ->
	    {NotOK, RC3}
    end.

call_controller_action(Adapter, AdapterInfo, RequestContext) ->
    try
        Adapter:action(AdapterInfo, RequestContext)
    catch
        Class:Error ->
            lager:error("Error in controller ~s", [boss_log_util:stacktrace(Class, Error)]),
            {error, "Error in controller, see console.log for details\n"}
    end.

make_action_session_id(Controller, AppInfo, Req, undefined, Adapter) ->
    case Adapter:wants_session(AppInfo#boss_app_info.application,
			       Controller,
			       AppInfo#boss_app_info.controller_modules) of
	true -> generate_session_id(Req);
	_    -> undefined
    end;
make_action_session_id(_Controller, _AppInfo, _Req, SessionID, _Adapter) ->
    SessionID.
    

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
