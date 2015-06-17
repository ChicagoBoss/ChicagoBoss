-module(boss_web_controller_render).

-export([render_error/4]).

-export([render_result/8]).

-export([render_errors/3]).

-export([render_view/3]).

-export([render_view/5]).

-include("boss_web.hrl").
-type action_results() :: 'render'|render_other|redirect| js| json| jsonp| output.

-spec apply_middle_filters(atom() | tuple(),_,_,_) -> any().
-spec choose_language_from_qvalues(_,_,[{[any()],float()}]) -> {_,_}.
-spec choose_translation_fun(_,_,_,_) -> {_,_}.
-spec expand_action_result(_) -> {action_results(), _,_,_ }|{action_results(), _,_ } |any().
-spec load_result(_,_,_,[any()]) -> any().
-spec process_action_result({_,_,nonempty_maybe_improper_list()},_,[],_) -> any().
-spec process_location(_,nonempty_maybe_improper_list(),#boss_app_info{}) -> {_,string(),_}.
-spec process_redirect(_,_,_) -> any().
-spec render_error(_,_,#boss_app_info{},[any()]) -> any().
-spec render_errors(_,#boss_app_info{},[any()]) -> any().
-spec render_result(_,_,_,_,_,_,_,_) -> any().
-spec render_view({_,_,_},_,[any()]) -> any().
-spec render_view({_,_,_},_,[any()],[]) -> any().
-spec render_view({_,_,_},_,[any()],_,_) -> any().
-spec render_with_template(_,_,#boss_app_info{application::atom()},[any()],_,_,atom() | tuple(),[any()],_,_,atom() | tuple()) -> any().
-spec translation_coverage(_,_,_) -> float().

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

render_result(Location, AppInfo, _RequestContext, LocationTrail, Adapter,
              AdapterInfo, ActionResult, RequestContext3) ->
    case ActionResult of
	undefined ->
	    render_view(Location, AppInfo, RequestContext3, [], []);
	_ ->
	    ExpandedResult	= expand_action_result(ActionResult),
	    TransformedResult	= apply_middle_filters(Adapter, AdapterInfo,
						     RequestContext3, ExpandedResult),
	    process_action_result({Location, RequestContext3, [Location|LocationTrail]},
				  TransformedResult, [], AppInfo)
    end.

apply_middle_filters(Adapter, AdapterInfo, RequestContext, ActionResult) ->
    GlobalFilters = boss_web_controller:filters_for_function('middle_filter'),
    ActionFilters = Adapter:filters('middle', AdapterInfo, RequestContext, GlobalFilters),

    lists:foldl(fun
		    (_Filter, {StatusCode, Payload, Headers}) when is_integer(StatusCode) ->
			{StatusCode, Payload, Headers};
		    (_Filter, {ok, Payload, Headers}) ->
			{ok, Payload, Headers};
		    (Filter, Result) when is_atom(Filter) ->
			{FilterKey, DefaultConfig} = boss_web_controller:filter_config(Filter),
			FilterConfig = Adapter:filter_config(AdapterInfo, FilterKey, DefaultConfig, RequestContext),
			case proplists:get_value(middle_filter, Filter:module_info(exports)) of
			    3 -> Filter:middle_filter(Result, FilterConfig, RequestContext);
			    _ -> Result
			end
		end, ActionResult, ActionFilters).


process_location(Controller,  [{_, _}|_] = Where, AppInfo) ->
    {_, TheController, TheAction, CleanParams} = process_redirect(Controller, Where, AppInfo),
    ControllerModule	= list_to_atom(boss_files:web_controller(
					 AppInfo#boss_app_info.application, 
					 Controller, 
					 AppInfo#boss_app_info.controller_modules)),
    ActionAtom		= to_atom(TheAction),
    {Tokens, []}	= boss_controller_lib:convert_params_to_tokens(CleanParams, ControllerModule, ActionAtom),
    {TheController, TheAction, Tokens}.

to_atom(A) when is_atom(A) -> A;
to_atom(L) when is_list(L) -> list_to_atom(L);
to_atom(B) when is_binary(B) -> list_to_atom(binary_to_list(B)).

process_redirect(Controller, [{_, _}|_] = Where, AppInfo) ->
    TheApplication	= proplists:get_value(application, Where, AppInfo#boss_app_info.application),
    TheController	= proplists:get_value(controller, Where, Controller),
    TheAction		= proplists:get_value(action, Where),
    CleanParams		= lists:foldl(fun(Key, Vars) ->
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
	expand_action_result({render_other, OtherLocation, []});
expand_action_result({render_other, [{_,_}|_]=OtherLocation, Data}) ->
    {render_other, OtherLocation, Data, []};
expand_action_result({render_other, OtherLocation, _Data}) ->
	lager:error("Action returned an invalid Location with render_other. Expected a proplist, but returned ~p", [OtherLocation]),
	{output, "bad return value from controller action\n", []};
expand_action_result({redirect, Where}) ->
    {redirect, Where, []};
expand_action_result({redirect, Where, Headers}) ->
    {redirect, Where, Headers};
expand_action_result({js, Data}) ->
    {js, Data, []};
expand_action_result({json, Data}) ->
    {json, Data, []};
expand_action_result({jsonp, Callback, Data}) ->
    {jsonp, Callback, Data, []};
expand_action_result({output, Payload}) ->
    {output, Payload, []};
expand_action_result({output, Payload, Headers}) ->
    {output, Payload, Headers};
expand_action_result({Directive, _}) when is_list(Directive) ->
    lager:error("Action returned an invalid return ~p should be an atom not a string", [Directive]),
    {output, "bad return value from controller action\n",[]};
expand_action_result({Directive,_, _}) when is_list(Directive) ->
    lager:error("Action returned an invalid return ~p should be an atom not a string", [Directive]),
    {output, "bad return value from controller action\n",[]};
expand_action_result(Other) ->
    Other.



process_action_result({Location, RequestContext, _},
		      {render, Data, Headers}, ExtraHeaders, AppInfo) ->
    render_view(Location, AppInfo, RequestContext, Data, boss_web_controller:merge_headers(Headers, ExtraHeaders));

process_action_result({{Controller, _, _}, RequestContext, _}, 
		      {render_other, OtherLocation, Data, Headers}, ExtraHeaders, AppInfo) ->
    TheApplication = proplists:get_value(application, OtherLocation, AppInfo#boss_app_info.application),
    TheAppInfo = boss_web:application_info(TheApplication),
    TheAppInfo1 = TheAppInfo#boss_app_info{ translator_pid = boss_web:translator_pid(TheApplication),
                                            router_pid = boss_web:router_pid(TheApplication) },
    render_view(process_location(Controller, OtherLocation, TheAppInfo1),
        TheAppInfo1, RequestContext, Data, boss_web_controller:merge_headers(Headers, ExtraHeaders));

process_action_result({{Controller, _, _}, RequestContext, LocationTrail}, {action_other, OtherLocation}, _, AppInfo) ->
    {Result, _} = boss_web_controller:execute_action(process_location(Controller, OtherLocation, AppInfo), AppInfo, RequestContext, LocationTrail),
    Result;

process_action_result({_, RequestContext, LocationTrail}, not_found, _, AppInfo) ->
    case boss_router:handle(AppInfo#boss_app_info.router_pid, 404) of
        {ok, {Application, Controller, Action, Params}} when Application =:= AppInfo#boss_app_info.application ->
            case boss_web_controller:execute_action({Controller, Action, Params}, AppInfo, RequestContext, LocationTrail) of
                Other ->
                    Other
            end;
        {ok, {OtherApplication, Controller, Action, Params}} ->
            {redirect, {OtherApplication, Controller, Action, Params}};
        not_found ->
            {not_found, "The requested page was not found. Additionally, no handler was found for processing 404 errors."}
    end;

process_action_result({{Controller, _, _}, _, _}, {redirect, Where, Headers}, ExtraHeaders, AppInfo) ->
    {redirect, process_redirect(Controller, Where, AppInfo), boss_web_controller:merge_headers(Headers, ExtraHeaders)};

process_action_result(Info, {js, Data, Headers}, ExtraHeaders, AppInfo) ->
    process_action_result(Info, {render, Data, boss_web_controller:merge_headers(Headers, [{"Content-Type", "application/javascript"}])},
			  ExtraHeaders, AppInfo);
process_action_result(Info, {json, Data, Headers}, ExtraHeaders, AppInfo) ->
    process_action_result(Info, {output, boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
				 boss_web_controller:merge_headers(Headers, [{"Content-Type", "application/json"}])}, ExtraHeaders, AppInfo);
process_action_result(Info, {jsonp, Callback, Data, Headers}, ExtraHeaders, AppInfo) ->
    JsonData  = boss_json:encode(Data, AppInfo#boss_app_info.model_modules),
    process_action_result(Info, {output, Callback ++ "(" ++ JsonData ++ ");",
				 boss_web_controller:merge_headers(Headers, [{"Content-Type", "application/javascript"}])}, ExtraHeaders, AppInfo);
process_action_result(_, {output, Payload, Headers}, ExtraHeaders, _) ->
    {ok, Payload, boss_web_controller:merge_headers(Headers, ExtraHeaders)};
process_action_result(_, Else, _, _) ->
    Else.

%% @desc generates HTML output for errors. called from load_and_execute/5
%% (seems to be called on parse error)
render_errors(ErrorList, AppInfo, RequestContext) ->
    case boss_html_errors_template:render(RequestContext ++ [
                {error, ErrorList}, 
                {'_base_url', AppInfo#boss_app_info.base_url},
                {'_static',   AppInfo#boss_app_info.static_prefix}]) of
        {ok, Payload} ->
            {ok, Payload, []};
        Err ->
	    lager:error("Unable to render boss_html_errors_template ~p",[Err]),
            Err
    end.

render_view(Location, AppInfo, RequestContext) ->
    render_view(Location, AppInfo, RequestContext, []).

render_view(Location, AppInfo, RequestContext, Variables) ->
    render_view(Location, AppInfo, RequestContext, Variables, []).

render_view({Controller, Template, _}, AppInfo, RequestContext, Variables, Headers) ->
    Req			= proplists:get_value(request, RequestContext),
    SessionID		= proplists:get_value(session_id, RequestContext),
    TryExtensions	= boss_files:template_extensions(),
 
    LoadResult		= load_result(Controller, Template, AppInfo, TryExtensions),
    BossFlash		= boss_flash:get_and_clear(SessionID),
    SessionData		= boss_session:get_session_data(SessionID),
    case LoadResult of
        {ok, Module, TemplateAdapter} ->
            render_with_template(Controller, Template, AppInfo, RequestContext,
                                 Variables, Headers, Req, BossFlash,
                                 SessionData, Module, TemplateAdapter);
        {error, not_found} ->
            AnyViewPath = boss_files_util:web_view_path(Controller, Template, "{" ++ string:join(TryExtensions, ",") ++ "}"),
            {not_found, io_lib:format("The requested template (~p) was not found.~n If you controller did not run, check that it was exported~n~n", [AnyViewPath]) };
        {error, {File, [{0, _Module, "Failed to read file"}]}} ->
            {not_found, io_lib:format("The requested template (~p) was not found.~n", [File]) };
        {error, Error = {ErrorType, EProblem, ELine}}->
            lager:error("Template \"~s\" has Error ~p: \"~p\" on line ~p", [Template,ErrorType, EProblem, ELine + 1 ]),
            render_errors([Error], AppInfo, RequestContext);
        {error, Error}->
            lager:error("Template Error template : ~p  error: ~p ", [Template,Error]),
            render_errors([Error], AppInfo, RequestContext)
    end.

render_with_template(Controller, Template, AppInfo, RequestContext,
                     Variables, Headers, Req, BossFlash, SessionData, Module,
                     TemplateAdapter) ->

    TranslatableStrings = TemplateAdapter:translatable_strings(Module),
    TranslatorPid = AppInfo#boss_app_info.translator_pid,
    AcceptLanguage = Req:header(accept_language),
    ContentLanguage = extract_content_language(RequestContext, Headers),
    
    {Lang, TranslationFun} = choose_translation_fun(TranslatorPid, TranslatableStrings,
						    AcceptLanguage, ContentLanguage),

    BeforeVars = case proplists:get_value('_before', RequestContext) of
                     undefined -> [];
                     AuthInfo -> [{"_before", AuthInfo}]
                 end,
    RenderVars0 = BossFlash ++ BeforeVars ++ [{"_lang", Lang}, {"_session", SessionData},
                                             {"_req", Req}, {"_base_url", AppInfo#boss_app_info.base_url} | Variables],
    RenderVars = [{"_vars", RenderVars0} | RenderVars0],
    RenderOptions = [
			{translation_fun, TranslationFun},
			{locale, Lang},
			{host, Req:header(host)},
			{application, atom_to_list(AppInfo#boss_app_info.application)},
			{controller, Controller},
			{action, Template},
			{router_pid, AppInfo#boss_app_info.router_pid}
		    ],

    try TemplateAdapter:render(Module, RenderVars, RenderOptions) of
	{ok, Payload} ->
	    MergedHeaders = boss_web_controller:merge_headers([{"Content-Language", Lang}], Headers),
	    {ok, Payload, MergedHeaders};
	Err ->
	    Err
    catch
        Class:Error ->
            lager:error("Error in view ~p ~s", [Module, boss_log_util:stacktrace(Class, Error)])
    end.

extract_content_language(RequestContext, Headers) ->
    case proplists:get_value(language, RequestContext) of
       undefined -> extract_content_language_from_headers(Headers);
       Lang -> Lang
    end.

extract_content_language_from_headers(Headers) ->
    case [V || {K,V} <- Headers, (is_list(K) andalso string:to_lower(K)=:="content-language")] of
	[Lang|_] -> Lang;
	[] -> undefined
    end.

load_result(Controller, Template, AppInfo, TryExtensions) ->
    lists:foldl(fun
		    (Ext, {error, not_found}) ->
			ViewPath = boss_files_util:web_view_path(Controller, Template, Ext),
			boss_load:load_view_if_dev(AppInfo#boss_app_info.application,
						   ViewPath, AppInfo#boss_app_info.view_modules,
				                   AppInfo#boss_app_info.translator_pid);
		    (_, Acc) ->
			Acc
                end, {error, not_found}, TryExtensions).

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
