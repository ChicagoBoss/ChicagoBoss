-module(boss_web_controller).
-export([start/0, start/1, stop/0, handle_request/3, process_request/1]).
-define(DEBUGPRINT(A), error_logger:info_report("~~o)> " ++ A)).

start() ->
    start([]).

start(Config) ->
    LogDir = get_env(log_dir, "log"),
    LogFile = make_log_file_name(LogDir),
    ok = error_logger:logfile({open, LogFile}),
    %ok = error_logger:tty(false),
    ok = make_log_file_symlink(LogFile),

    Env = boss_load:setup_boss_env(),
    error_logger:info_msg("Starting Boss in ~p mode....~n", [Env]),

    boss_db:start(),
    boss_session:start(),
    boss_mq:start(),
	
    MailDriver = get_env(mail_driver, boss_mail_driver_smtp),
    boss_mail:start([{driver, MailDriver}]),

    boss_translator:start(),
    {ServerMod, RequestMod, ResponseMod} = case application:get_env(server) of
        {ok, mochiweb} -> {mochiweb_http, mochiweb_request_bridge, mochiweb_response_bridge};
        _ -> {misultin, misultin_request_bridge, misultin_response_bridge}
    end,
    ServerConfig = [{loop, fun(Req) -> ?MODULE:handle_request(Req, RequestMod, ResponseMod) end} | Config],
    case ServerMod of
        mochiweb_http -> mochiweb_http:start(ServerConfig);
        misultin -> misultin:start_link(ServerConfig)
    end.

stop() ->
    error_logger:logfile(close),
    boss_mq:stop(),
    boss_session:stop(),
    boss_db:stop(),
    mochiweb_http:stop(),
    misultin:stop().

handle_request(Req, RequestMod, ResponseMod) ->
    DocRoot = boss_files:static_path(),
    Request = simple_bridge:make_request(RequestMod, {Req, DocRoot}),
    case Request:path() of
        "/favicon.ico" ->
            Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
            (Response:file("/favicon.ico")):build_response();
        "/static/"++File -> 
            Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
            (Response:file([$/|File])):build_response();
        "/admin/static/"++File ->
            Response = simple_bridge:make_response(ResponseMod, {Req, boss_files:admin_static_path()}),
            (Response:file([$/|File])):build_response();
        _ -> 
            SessionKey = boss_session:get_session_key(),
            SessionId = boss_session:new_session(Request:cookie(SessionKey)),			
            {StatusCode, Headers, Payload} = process_request(Request),
            ErrorFormat = "~s ~s ~p~n", 
            ErrorArgs = [Request:request_method(), Request:path(), StatusCode],
            case StatusCode of
                500 -> error_logger:error_msg(ErrorFormat, ErrorArgs);
                404 -> error_logger:warning_msg(ErrorFormat, ErrorArgs);
                _ -> error_logger:info_msg(ErrorFormat, ErrorArgs)
            end,
            Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
            Response1 = (Response:status_code(StatusCode)):data(Payload),
            SessionExpTime = boss_session:get_session_exp_time(),
            Response2 = Response1:cookie(SessionKey, SessionId, "/", SessionExpTime),
            Response3 = lists:foldl(fun({K, V}, Acc) -> Acc:header(K, V) end, Response2, Headers),
            Response3:build_response()
    end.

process_request(Req) ->
    Result = case parse_path(Req:path()) of
        {ok, {Controller, Action, Tokens}} ->
            trap_load_and_execute({Controller, Action, Tokens}, Req);
        Else ->
            Else
    end,
    process_result(Result).

get_env(Key, Default) when is_atom(Key) ->
    case application:get_env(Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

parse_path("/") ->
    {Controller, Action} = get_env(front_page, {"hello", "world"}), 
    {ok, {Controller, Action, []}};
parse_path("/" ++ Url) ->
    Tokens = string:tokens(Url, "/"),
    case length(Tokens) of
        1 ->
            Controller = hd(Tokens),
            DefaultAction = get_env(default_action, "index"),
            AllDefaultActions = get_env(default_actions, [{"admin", "model"}]),
            ThisAction = proplists:get_value(Controller, AllDefaultActions, DefaultAction),
            {ok, {Controller, ThisAction, []}};
        N when N >= 2 ->
            {ok, {lists:nth(1, Tokens), 
                    lists:nth(2, Tokens),
                    lists:nthtail(2, Tokens)}};
        _ ->
            {not_found, "File not found"}
    end;
parse_path(_) ->
    {not_found, "File not found"}.

process_result({error, Payload}) ->
    error_logger:error_report(Payload),
    {500, [{"Content-Type", "text/html"}], "Error: <pre>" ++ io_lib:print(Payload) ++ "</pre>"};
process_result({not_found, Payload}) ->
    {404, [{"Content-Type", "text/html"}], Payload};
process_result({redirect, Where}) ->
    process_result({redirect, Where, []});
process_result({redirect, "http://"++Where, Headers}) ->
    process_result({redirect, "/"++string:join(tl(string:tokens(Where, "/")), "/"), Headers});
process_result({redirect, Where, Headers}) ->
    {302, [{"Location", Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result({ok, Payload, Headers}) ->
    {200, [{"Content-Type", proplists:get_value("Content-Type", Headers, "text/html")}
            |proplists:delete("Content-Type", Headers)], Payload}.

trap_load_and_execute(Arg1, Arg2) ->
    case catch load_and_execute(Arg1, Arg2) of
        {'EXIT', Reason} ->
            {error, Reason};
        Ok ->
            Ok
    end.

load_and_execute(Location, Req) ->
    case boss_load:boss_env() of
        production -> load_and_execute_prod(Location, Req);
        testing -> load_and_execute_dev(Location, Req);
        _ -> load_and_execute_dev(Location, Req)
    end.

load_and_execute_prod({Controller, _, _} = Location, Req) ->
    case lists:member(Controller ++ "_controller", boss_files:web_controller_list()) of
        true -> execute_action(Location, Req);
        false -> render_view(Location, Req)
    end.

load_and_execute_dev({"doc", ModelName, _}, Req) ->
    case string:chr(ModelName, $.) of
        0 ->
            case boss_load:load_models() of
                {ok, _} ->
                    Model = list_to_atom(ModelName),
                    {Model, Edoc} = boss_record_compiler:edoc_module(
                        boss_files:model_path(ModelName++".erl"), [{private, true}]),
                    {ok, edoc:layout(Edoc), []};
                {error, ErrorList} ->
                    render_errors(ErrorList, Req)
            end;
        _ ->
            {not_found, "File not found"}
    end;
load_and_execute_dev({Controller, _, _} = Location, Req) ->
    case boss_load:load_mail_controllers() of
        {ok, _} -> 
            case boss_load:load_libraries() of
                {ok, _} ->
                    case boss_load:load_web_controllers() of
                        {ok, Controllers} ->
                            case lists:member(Controller ++ "_controller", Controllers) of
                                true ->
                                    case boss_load:load_models() of
                                        {ok, _} ->
                                            execute_action(Location, Req);
                                        {error, ErrorList} ->
                                            render_errors(ErrorList, Req)
                                    end;
                                false ->
                                    render_view(Location, Req)
                            end;
                        {error, ErrorList} when is_list(ErrorList) ->
                            render_errors(ErrorList, Req)
                    end;
                {error, ErrorList} ->
                    render_errors(ErrorList, Req)
            end;
        {error, ErrorList} ->
            render_errors(ErrorList, Req)
    end.

render_errors(ErrorList, Req) ->
    render_view({"admin", "error", []}, Req, [{errors, ErrorList}]).

execute_action(Location, Req) ->
    execute_action(Location, Req, []).

execute_action({Controller, Action}, Req, LocationTrail) ->
    execute_action({Controller, Action, []}, Req, LocationTrail);
execute_action({Controller, Action, Tokens}, Req, LocationTrail) when is_atom(Action) ->
    execute_action({Controller, atom_to_list(Action), Tokens}, Req, LocationTrail);
execute_action({Controller, Action, Tokens} = Location, Req, LocationTrail) ->
    case lists:member(Location, LocationTrail) of
        true ->
            {error, "Circular redirect!"};
        _ ->
            % do not convert a list to an atom until we are sure the controller/action
            % pair exists. this prevents a memory leak due to atom creation.
            Module = list_to_atom(lists:concat([Controller, "_controller"])),
            ControllerInstance = Module:new(Req),
            ExportStrings = lists:map(
                fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
                Module:module_info(exports)),
            AuthInfo = case lists:member({"before_", 2}, ExportStrings) of
                true -> ControllerInstance:before_(Action);
                false -> {ok, undefined}
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
                            render_view(Location, Req, [{"before_", Info}]);
                        ActionResult ->
                            process_action_result({Location, Req, LocationTrail}, ActionResult, Info)
                    end,
                    case proplists:get_value("after_", ExportStrings) of
                        3 ->
                            ControllerInstance:after_(Action, Result);
                        4 ->
                            ControllerInstance:after_(Action, Result, Info);
                        _ ->
                            Result
                    end;
                Other ->
                    Other
            end
    end.

process_action_result(Info, ok, AuthInfo) ->
    process_action_result(Info, {ok, []}, AuthInfo);
process_action_result(Info, {ok, Data}, AuthInfo) ->
    process_action_result(Info, {ok, Data, []}, AuthInfo);
process_action_result({Location, Req, _}, {ok, Data, Headers}, AuthInfo) ->
    render_view(Location, Req, [{"before_", AuthInfo}|Data], Headers);

process_action_result(Info, {render_other, OtherLocation}, AuthInfo) ->
    process_action_result(Info, {render_other, OtherLocation, []}, AuthInfo);
process_action_result(Info, {render_other, OtherLocation, Data}, AuthInfo) ->
    process_action_result(Info, {render_other, OtherLocation, Data, []}, AuthInfo);
process_action_result(Info, {render_other, {Controller, Action}, Data, Headers}, AuthInfo) ->
    process_action_result(Info, {render_other, {Controller, Action, []}, Data, Headers}, AuthInfo);
process_action_result({_, Req, _}, {render_other, {_, _, _} = OtherLocation, Data, Headers}, AuthInfo) ->
    render_view(OtherLocation, Req, [{"before_", AuthInfo}|Data], Headers);

process_action_result({_, Req, LocationTrail}, {action_other, OtherLocation}, _) ->
    execute_action(OtherLocation, Req, [OtherLocation | LocationTrail]);

process_action_result(Info, {json, Data}, AuthInfo) ->
    process_action_result(Info, {json, Data, []}, AuthInfo);
process_action_result(Info, {json, Data, Headers}, AuthInfo) ->
    process_action_result(Info, {output, json_data(Data),
            [{"Content-Type", proplists:get_value("Content-Type", Headers, "application/json")}
                |proplists:delete("Content-Type", Headers)]}, AuthInfo);

process_action_result(Info, {output, Payload}, AuthInfo) ->
    process_action_result(Info, {output, Payload, []}, AuthInfo);
process_action_result(_, {output, Payload, Headers}, _) ->
    {ok, Payload, Headers};

process_action_result(_, Else, _) ->
    Else.

render_view(Location, Req) ->
    render_view(Location, Req, []).

render_view(Location, Req, Variables) ->
    render_view(Location, Req, Variables, []).

render_view({Controller, Template, _}, Req, Variables, Headers) ->
    ViewPath = boss_files:web_view_path(Controller, Template),
    LoadResult = boss_load:load_view_if_dev(ViewPath),
    BossFlash = boss_flash:get_and_clear(Req),
    case LoadResult of
        {ok, Module} ->
            TranslationFun = choose_translation_fun(Module:translatable_strings(), 
                Req:header(accept_language), proplists:get_value("Content-Language", Headers)),
            case Module:render(lists:merge(Variables, BossFlash), TranslationFun) of
                {ok, Payload} ->
                    {ok, Payload, Headers};
                Err ->
                    Err
            end;
        {error, Error}-> 
            render_errors([Error], Req)
    end.

choose_translation_fun(_, undefined, undefined) ->
    none;
choose_translation_fun(Strings, AcceptLanguages, undefined) ->
    case mochiweb_util:parse_qvalues(AcceptLanguages) of
        invalid_qvalue_string ->
            none;
        [{Lang, _}] ->
            translation_fun_for(Lang);
        QValues when length(QValues) > 1 ->
            {BestLang, BestNetQValue} = choose_language_from_qvalues(Strings, QValues),
            case BestNetQValue of
                0.0 -> none;
                _ -> translation_fun_for(BestLang)
            end
    end;
choose_translation_fun(_, _, ContentLanguage) ->
    boss_translator:fun_for(ContentLanguage).

choose_language_from_qvalues(Strings, QValues) ->
    % calculating translation coverage is costly so we start with the most preferred
    % languages and work our way down
    SortedQValues = lists:reverse(lists:keysort(2, QValues)),
    AssumedLocale = get_env(assume_locale, "en"),
    AssumedLocaleQValue = proplists:get_value(AssumedLocale, SortedQValues, 0.0),
    lists:foldl(
        fun
            ({_, ThisQValue}, {BestLang, BestTranslationScore}) when BestTranslationScore >= ThisQValue ->
                {BestLang, BestTranslationScore};
            ({ThisLang, ThisQValue}, {_, BestTranslationScore}) when ThisLang =:= AssumedLocale andalso
                                                                     ThisQValue > BestTranslationScore ->
                {ThisLang, ThisQValue}; % translation coverage is 100%
            ({ThisLang, ThisQValue}, {BestLang, BestTranslationScore}) ->
                TranslationCoverage = translation_coverage(Strings, ThisLang),
                TranslationScore = ThisQValue * TranslationCoverage + 
                                    AssumedLocaleQValue * (1-TranslationCoverage),
                case TranslationScore > BestTranslationScore of
                    true -> {ThisLang, TranslationScore};
                    false -> {BestLang, BestTranslationScore}
                end
        end, {"xx-bork", 0.0}, SortedQValues).

translation_fun_for(Locale) ->
    fun(String) -> boss_translator:lookup(String, Locale) end.

translation_coverage([], _) ->
    0.0;
translation_coverage(Strings, Locale) ->
    case boss_translator:is_loaded(Locale) of
        true ->
            lists:foldl(fun(String, Acc) ->
                        case boss_translator:lookup(String, Locale) of
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

json_data(Data) ->
    mochijson2:encode({struct, json_data1(Data, [])}).

json_data1([], Acc) ->
    lists:reverse(Acc);
json_data1([{VariableName, [First|_] = Variable}|Rest], Acc) ->
    case boss_record_lib:is_boss_record(First) of
        true ->
            json_data1(Rest, [{VariableName, lists:map(fun boss_record_to_json/1, Variable)}|Acc]);
        false ->
            json_data1(Rest, [{VariableName, Variable}|Acc])
    end;
json_data1([{VariableName, Variable}|Rest], Acc) ->
    case boss_record_lib:is_boss_record(Variable) of
        true -> 
            Struct = boss_record_to_json(Variable),
            json_data1(Rest, [{VariableName, Struct}|Acc]);
        false -> 
            json_data1(Rest, [{VariableName, Variable}|Acc])
    end.

boss_record_to_json(Variable) ->
    Data = lists:map(fun
            ({Attr, Val}) when is_list(Val) ->
                {Attr, list_to_binary(Val)};
            ({Attr, {{_, _, _}, {_, _, _}} = Val}) ->
                {Attr, list_to_binary(erlydtl_filters:date(Val, "F d, Y H:i:s"))};
            (Other) ->
                Other
        end, Variable:attributes()),
    {struct, Data}.
