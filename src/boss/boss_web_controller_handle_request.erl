-module(boss_web_controller_handle_request).

-export([handle_request/4]).

-export([process_request/5]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("boss_web.hrl").

%% TODO REFACTOR AND TEST
handle_request(Req, RequestMod, ResponseMod, RouterAdapter) ->
  
    LoadedApplications	= boss_web:get_all_applications(),
    Request		= simple_bridge:make_request(RequestMod, Req),
    FullUrl		= Request:path(),

    ApplicationForPath	= RouterAdapter:find_application_for_path(Request,
                                                                  FullUrl, 
                                                                  LoadedApplications),
    lager:notice("ApplicationForPath ~p~n", [ApplicationForPath]),
    
    try
	   handle_application(Req, ResponseMod, Request, FullUrl, ApplicationForPath, RouterAdapter)
    catch Class:Error ->
    	%% Nuclear option: Something very serious happened and we don't want to
    	%% fail silently, but instead it should generate an error message.
    	lager:error("Unhandled Error: ~s", [boss_log_util:stacktrace(Class, Error)]),
    	handle_fatal_error(Req, ResponseMod)
    end.

handle_fatal_error(Req, ResponseMod) ->
	Response  = simple_bridge:make_response(ResponseMod, {Req, undefined}),
	Response1 = (Response:status_code(500)):data(["An unhandled and unrecoverable error occurred. Please check error logs."]),
	Response1:build_response().

handle_application(Req, ResponseMod, _Request, _FullUrl, undefined, _RouterAdapter) ->
    Response	 = simple_bridge:make_response(ResponseMod, {Req, undefined}),
    Response1	 = (Response:status_code(404)):data(["No application configured at this URL"]),
    Response1:build_response();
handle_application(Req, ResponseMod, Request, FullUrl,  App, RouterAdapter) ->
    BaseURL		 = boss_web:base_url(App),
    DocRoot      = boss_files_util:static_path(App),
    StaticPrefix = boss_web:static_prefix(App),
    Url			 = lists:nthtail(length(BaseURL), FullUrl),
    Response	 = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
    SpecialFiles = boss_env:get_env(App,
            					   static_files,
            					   [
                                    "/favicon.ico", 
                                    "/apple-touch-icon.png", 
                                    "/robots.txt"
                                   ]),
    IsSpecialFile= lists:member(Url,SpecialFiles),
   
    handle_result(Request, App, StaticPrefix, Url, Response, IsSpecialFile, RouterAdapter).


handle_result(_Request, _App, _StaticPrefix, Url, Response, _IsSpecialFile = true, _) ->
    (Response:file(Url)):build_response();
handle_result(Request, App, StaticPrefix, Url, Response, _IsSpecialFile = false, RouterAdapter) ->
    TestStaticPrefix = string:substr(Url, 1, length(StaticPrefix)),
    case TestStaticPrefix of
	StaticPrefix ->
	    build_static_response(App, StaticPrefix, Url, Response);
	_ ->
	    build_dynamic_response(App, Request, Response, Url, RouterAdapter)
    end.

build_static_response(App, StaticPrefix, Url, Response) ->
    [$/ |File]	 = lists:nthtail(length(StaticPrefix), Url),
    IsDevelopment= boss_env:boss_env(),
    Sha1		 = make_etag(App, StaticPrefix, File),
    Ops          = [
            	    fun(Resp) -> dev_headers(Resp, IsDevelopment) end,
            	    fun(Resp) ->  Resp:file([$/ |File])	end,
            	    fun(Resp) ->  case Sha1 of 
            	                  	{error, _} -> Resp;
            	                  	_          -> Resp:header("Etag",Sha1)
            	                  end end,
            	    fun(Resp) ->  Resp:build_response()	end
           	       ],

    lists:foldl(fun(Operation, Resp) ->
			         Operation(Resp) 
		        end, 
                Response, 
                Ops).

    

-spec(dev_headers(any(), production|development)-> any()).
dev_headers(Response, development) ->
    Response:header("Cache-Control", "no-cache");
dev_headers(Response, _) ->
    Response.
    

make_etag(App, StaticPrefix, File) ->
	Priv = case code:priv_dir(App) of
		{error, bad_name} ->
			%% enuit isn't loading the application, so this will default for us
			"../priv";
		P ->
			P
	end,
    FilePath      = Priv ++ "/" ++ StaticPrefix ++ "/" ++ File,
    case file:read_file(FilePath) of
    	{ok, Content} -> 
    		binary_to_list(base64:encode(crypto:hash(sha, Content)));
    	{error, enoent} ->
    		 lager:warning("application ~s file ~s not found", [App, FilePath]),
    		 {error, enoent};
    	Err ->
    		Err
    end.
    

%% TODO: Refactor
build_dynamic_response(App, Request, Response, Url, RouterAdapter) ->
    Mode            = boss_web_controller_util:execution_mode(App),
    AppInfo		    = boss_web:application_info(App),

    TranslatorPid	= boss_web:translator_pid(App),
    RouterPid		= boss_web:router_pid(App),
    ControllerList	= boss_files:web_controller_list(App),
    TR              = set_timer(Request, 
                                Url, 
                                Mode,
				                AppInfo, 
                                TranslatorPid,
				                RouterPid,
				                ControllerList,
                                RouterAdapter
                                ),
    {Time, {StatusCode, Headers, Payload}} = TR,
    ErrorFormat		= "~s ~s [~p] ~p ~pms",
    RequestMethod	= Request:request_method(),
    FullUrl		= Request:path(),
    ErrorArgs		= [RequestMethod, FullUrl, App, StatusCode, Time div 1000],
    log_status_code(StatusCode, ErrorFormat, ErrorArgs),
    Response1		= (Response:status_code(StatusCode)):data(Payload),
    Response2		= lists:foldl(fun({K, V}, Acc) ->
					      Acc:header(K, V) 
				      end, 
				      Response1, 
				      Headers),
    handle_response(Request, Payload, RequestMethod, Response2).

set_timer(Request, Url, Mode, AppInfo, TranslatorPid, RouterPid,
          ControllerList, RouterAdapter) ->
    NewAppInfo = AppInfo#boss_app_info{
		   translator_pid            = TranslatorPid,
		   router_pid                = RouterPid,
		   controller_modules        = ControllerList
		  },
    timer:tc(?MODULE,process_request,[NewAppInfo, Request, Mode, Url, RouterAdapter]).

handle_response(Request, _Payload = {stream, Generator, Acc0}, RequestMethod, Response2) ->
    Protocol		= Request:protocol_version(),
    TransferEncoding	= handle_protocol(Protocol),
    Response3		= Response2:data(chunked),
    Response3:build_response(),
    process_stream_generator(Request, TransferEncoding, RequestMethod, Generator, Acc0);
handle_response(_Request, Payload , _RequestMethod, Response2) ->
    (Response2:data(Payload)):build_response().


handle_protocol({1,1}) -> chunked;
handle_protocol(_)     -> identity.



log_status_code(500, ErrorFormat, ErrorArgs) ->
    error_logger:error_msg(ErrorFormat, ErrorArgs);
log_status_code(404, ErrorFormat, ErrorArgs) ->
    error_logger:warning_msg(ErrorFormat, ErrorArgs);
log_status_code(_, ErrorFormat, ErrorArgs) ->
    error_logger:info_msg(ErrorFormat, ErrorArgs).

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


process_request(#boss_app_info{ doc_prefix = DocPrefix } = AppInfo, Req, development, DocPrefix, _RouterAdapter) ->
    {Result, SessionID1} = case catch load_and_execute(development, {"doc", [], []}, AppInfo, [{request, Req}]) of
        {'EXIT', Reason} ->
            {{error, Reason}, boss_web_controller:generate_session_id(Req)};
        {R, S} ->
            {R, S}
    end,
    process_result_and_add_session(AppInfo, [{request, Req}, {session_id, SessionID1}], Result);
process_request(AppInfo, Req, development, Url, RouterAdapter) ->
  
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
            RouterAdapter:set_controllers(RouterPid, ControllerList),
            RouterAdapter:reload(RouterPid),
            process_dynamic_request(AppInfo, Req, development, Url, RouterAdapter)
    end,
    process_result_and_add_session(AppInfo, [{request, Req}, {session_id, SessionID1}], Result);
process_request(AppInfo, Req, Mode, Url, RouterAdapter) ->
    {Result, SessionID1} = process_dynamic_request(AppInfo, Req, Mode, Url, RouterAdapter),
    process_result_and_add_session(AppInfo, [{request, Req}, {session_id, SessionID1}], Result).

process_dynamic_request(#boss_app_info{ router_pid = RouterPid } = AppInfo, Req, Mode, Url, RouterAdapter) ->
    
    {Result, SessionID1} = case RouterAdapter:route(RouterPid, Url) of
            			       {ok, {Application, Controller, Action, Tokens}} when Application =:= AppInfo#boss_app_info.application ->
            				   Location = {Controller, Action, Tokens},
            				  
            				   RequestContext = [{request, Req}], 
            				   ExecuteResults = load_and_execute(Mode, Location, AppInfo, RequestContext),

            				   case  ExecuteResults of
            				       {'EXIT', Reason} ->
            					   {{error, Reason}, undefined};
            				       {{not_found, Message}, S1} ->
            					   {process_not_found(Message, AppInfo, [{request, Req}, {session_id, S1}], Mode, RouterAdapter), S1};
            				       {not_found, S1} ->
            					   {process_not_found("File not found.", AppInfo, [{request, Req}, {session_id, S1}], Mode, RouterAdapter), S1};
            				       Ok ->
            					   Ok
            				   end;
            			       {ok, {OtherApplication, Controller, Action, Tokens}} ->
            				   {{redirect, {OtherApplication, Controller, Action, Tokens}}, undefined};
            			       not_found ->
            				   {process_not_found("No routes matched the requested URL.", AppInfo, [{request, Req}], Mode, RouterAdapter),
            				    undefined}
            			   end,
    FinalResult = case Result of
		      {error, Payload} ->
            process_error(Payload, AppInfo, [{request, Req}, {session_id, SessionID1}], Mode, RouterAdapter);
        _ ->
            Result
    end,
    {FinalResult, SessionID1}.

process_not_found(Message, #boss_app_info{ router_pid = RouterPid } = AppInfo, RequestContext, Mode, RouterAdapter) ->
    case RouterAdapter:handle(RouterPid, 404) of
        {ok, {Application, Controller, Action, Tokens}} when Application =:= AppInfo#boss_app_info.application ->
            Location = {Controller, Action, Tokens},
            case catch load_and_execute(Mode, Location, AppInfo, RequestContext) of
                {'EXIT', Reason} ->
                    {error, Reason};
                {{ok, Payload, Headers}, _} ->
                    {not_found, Payload, Headers};
                {{Code, Payload, Headers}, _} ->
                    {Code, Payload, Headers}
            end;
        {ok, OtherLocation} ->
            {redirect, OtherLocation};
        not_found ->
            {not_found, [Message, " ",
                    "Additionally, no handler was found for processing 404 errors. ",
                    "You probably want to modify ", boss_files:routes_file(AppInfo#boss_app_info.application), " to prevent errors like this one."]}
    end.

process_error(Payload, AppInfo, RequestContext, development, RouterAdapter) ->
    error_logger:error_report(Payload),
    ExtraMessage = case RouterAdapter:handle(AppInfo#boss_app_info.router_pid, 500) of
        not_found ->
            ["This message will appear in production; you may want to define a 500 handler in ", boss_files:routes_file(AppInfo#boss_app_info.application)];
        _Route ->
            "(Don't worry, this message will not appear in production.)"
    end,
    boss_web_controller_render:render_error(io_lib:print(Payload), ExtraMessage, AppInfo, RequestContext);
    
process_error(Payload, #boss_app_info{ router_pid = RouterPid } = AppInfo, RequestContext, Mode, RouterAdapter) ->
    error_logger:error_report(Payload),
    case RouterAdapter:handle(RouterPid, 500) of
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
    Secure		    = boss_env:get_env(session_cookie_secure, false),
    CookieOptions3	= lists:merge(CookieOptions2, [{http_only, HttpOnly},
							                       {secure, Secure}]),
    SessionKey		= boss_session:get_session_key(),
    lists:merge(Headers, [mochiweb_cookies:cookie(SessionKey, SessionID, CookieOptions3)]).


%TODO: Refactor this
process_result(AppInfo, Req, {Status, Payload}) ->
    process_result(AppInfo, Req, {Status, Payload, []});
process_result(_, _, {ok, Payload, Headers}) ->
    {200, boss_web_controller:merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(AppInfo, Req, {stream, Generator, Acc0}) ->
    process_result(AppInfo, Req, {stream, Generator, Acc0, []});
process_result(_, _, {stream, Generator, Acc0, Headers}) ->
    {200, boss_web_controller:merge_headers(Headers, [{"Content-Type", "text/html"}]), {stream, Generator, Acc0}};
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
% allow external redirect with absolute url
process_result(AppInfo, Req, {redirect, "http://"++Where, Headers}) ->
    process_result(AppInfo, Req, {redirect_external, "http://"++Where, Headers});
process_result(AppInfo, Req, {redirect, "https://"++Where, Headers}) ->
    process_result(AppInfo, Req, {redirect_external, "https://"++Where, Headers});
% allow internal redirect with relative urls
process_result(AppInfo, Req, {redirect, "/"++Where, Headers}) ->
    process_result(AppInfo, Req, {redirect_external, "/"++Where, Headers});
process_result(AppInfo, Req, {redirect, "./"++Where, Headers}) ->
    process_result(AppInfo, Req, {redirect_external, "./"++Where, Headers});
process_result(AppInfo, Req, {redirect, "../"++Where, Headers}) ->
    process_result(AppInfo, Req, {redirect_external, "../"++Where, Headers});
process_result(AppInfo, Req, {redirect, {Application, Controller, Action, Params}, Headers}) ->
    {RouterPid, AppInfo1, Application1} = if
        AppInfo#boss_app_info.application =:= Application ->
            {AppInfo#boss_app_info.router_pid, AppInfo, Application};
        true ->
            {boss_web:router_pid(Application), boss_web:application_info( Application ), Application}
    end,
    ExtraParams = [{application, Application1}, {controller, Controller}, {action, Action}],
    URL = boss_erlydtl_tags:url(ExtraParams ++ Params, [
            {host, Req:header(host)},
            {base_url, AppInfo1#boss_app_info.base_url},
            {application, AppInfo1#boss_app_info.application},
            {router_pid, RouterPid}]),
    {302, [{"Location", URL}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(AppInfo, _, {redirect, Where, Headers}) ->
    {302, [{"Location", AppInfo#boss_app_info.base_url ++ Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(_, _, {redirect_external, Where, Headers}) ->
    {302, [{"Location", Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result(_, _, {unauthorized, Payload, Headers}) ->
    {401, boss_web_controller:merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(_, _, {not_found, Payload, Headers}) ->
    {404, boss_web_controller:merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(_, _, {error, Payload, Headers}) ->
    {500, boss_web_controller:merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload};
process_result(_, _, {StatusCode, Payload, Headers}) when is_integer(StatusCode) ->
    {StatusCode, boss_web_controller:merge_headers(Headers, [{"Content-Type", "text/html"}]), Payload}.
	
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
                        boss_files_util:model_path(ModelName ++ ".erl"), [{private, true}]),
                    {ok, correct_edoc_html(Edoc, AppInfo), []};
                false ->
                    % ok, it's not model, so it could be web controller
                    {ok, Controllers} = boss_load:load_web_controllers(AppInfo#boss_app_info.application),
                    case lists:member(ModelName, lists:map(fun atom_to_list/1, Controllers)) of
                            true ->
                                Controller = list_to_atom(ModelName),  
                                {Controller, Edoc} = edoc:get_doc(boss_files_util:web_controller_path(ModelName ++ ".erl"), [{private, true}]),
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

load_and_execute(development,
		 {Controller, _, _} = Location,
		 AppInfo = #boss_app_info{ application = Application}, 
		 RequestContext) ->
    SessionID = proplists:get_value(session_id, RequestContext),
    
    Ops   = [fun boss_load:load_mail_controllers/1, 
    	     fun boss_load:load_libraries/1, 
    	     fun boss_load:load_view_lib_modules/1,
    	     fun boss_load:load_services_websockets/1, 
    	     fun boss_load:load_web_controllers/1],
    Res  = fold_operations(Application, Ops),
    run_controller(Controller, Location, AppInfo, Application,
                          RequestContext, SessionID, Res).
    

run_controller(Controller, Location, AppInfo, Application,
               RequestContext, SessionID, {ok,Controllers}) ->
    
    ControllerNames   = make_controller_names(Controllers),
    
    ControllerPresent = boss_files:is_controller_present(Application, Controller,
					                 ControllerNames),
    
    run_controller_if_present(Location, AppInfo, Application,
	                      RequestContext, SessionID, ControllerPresent);
run_controller(_Controller, _Location, AppInfo, _Application,
               RequestContext, SessionID, {error, ErrorList}) ->
     {boss_web_controller_render:render_errors(ErrorList, AppInfo, RequestContext), SessionID}.

make_controller_names(Controllers) ->
    lists:map(fun atom_to_list/1, Controllers).


run_controller_if_present(Location, AppInfo, Application,
                          RequestContext, SessionID, true) ->
	    case boss_load:load_models(Application) of
		{ok, _} ->
		    execute_action(Location, AppInfo, RequestContext);
		{error, ErrorList} ->
		    {boss_web_controller_render:render_errors(ErrorList, AppInfo, RequestContext), 
		     SessionID}
	    end;
run_controller_if_present(Location, AppInfo, _Application,
                          RequestContext, SessionID, false) ->
	    {boss_web_controller_render:render_view(Location, AppInfo, RequestContext), SessionID}.


-spec(fold_operations(types:application(), 
		      [fun((types:application()) ->
				 {ok, _}|{error,_})]) ->
			      {ok,_}| {error, _}).
fold_operations(Application, Ops) ->
    lists:foldl(fun(Operation, {ok, _}) ->
			Operation(Application);
		   (_, Acc = {error,_}) ->
		        Acc
		end,
		{ok, start},
                Ops).

%% @desc function to correct path errors in HTML output produced by Edoc
correct_edoc_html(Edoc, AppInfo) ->
    Result = edoc:layout(Edoc, [{stylesheet, AppInfo#boss_app_info.base_url++AppInfo#boss_app_info.static_prefix++"/edoc/stylesheet.css"}]),
    Result2 = re:replace(Result, "overview-summary.html", "./", [{return,list}, global]),
    Result3 = re:replace(Result2, "erlang.png", AppInfo#boss_app_info.base_url++AppInfo#boss_app_info.static_prefix++"/edoc/erlang.png", [{return,list}, global]),
    Result3.

execute_action(Location, AppInfo, RequestContext) ->
    boss_web_controller:execute_action(Location, AppInfo, RequestContext, []).
