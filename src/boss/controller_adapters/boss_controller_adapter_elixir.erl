-module(boss_controller_adapter_elixir).
-compile(export_all).

convert_tokens(undefined) -> [];
convert_tokens(Tokens) -> lists:map(fun list_to_binary/1, Tokens).

convert_session_id(undefined) -> undefined;
convert_session_id(SessionID) -> list_to_binary(SessionID).

accept(Application, Controller, ControllerList) ->
    Module = boss_compiler_adapter_elixir:controller_module(Application, Controller),
    lists:member(Module, ControllerList).

wants_session(_Application, _Controller, _ControllerList) ->
    true.

init(Application, Controller, ControllerList, _RequestContext) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    ExportStrings = lists:map(
        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
        Module:module_info(exports)),
    {Module, ExportStrings}.

filters(Type, {Module, ExportStrings}, RequestContext, GlobalFilters) ->
    FunctionString = lists:concat([Type, "_filters"]),
    case proplists:get_value(FunctionString, ExportStrings) of
        2 -> 
            FunctionAtom = list_to_atom(FunctionString),
            Module:FunctionAtom(GlobalFilters, RequestContext);
        _ -> GlobalFilters
    end.

before_filter({Module, ExportStrings}, RequestContext) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    Tokens = proplists:get_value(tokens, RequestContext),
    RequestMethod = proplists:get_value(method, RequestContext),

    BinTokens = convert_tokens(Tokens),
    AuthResult = case proplists:get_value("before_", ExportStrings) of
        3 -> Module:before_(Req, SessionID, Action);
        5 -> Module:before_(Req, SessionID, Action, RequestMethod, BinTokens);
        _ -> no_before_function
    end,
    case AuthResult of
        no_before_function ->
            {ok, RequestContext};
        ok ->
            {ok, [{'_before', undefined}|RequestContext]};
        {ok, Info} ->
            {ok, [{'_before', Info}|RequestContext]};
        Other ->
            Other
    end.

after_filter({Module, ExportStrings}, RequestContext, Result) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext),
    
    case proplists:get_value("after_", ExportStrings) of
        4 -> Module:after_(Req, SessionID, Action, Result);
        5 -> Module:after_(Req, SessionID, Action, Result, AuthInfo);
        _ -> Result
    end.

action({Module, ExportStrings}, RequestContext) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    Tokens = proplists:get_value(tokens, RequestContext),
    RequestMethod = proplists:get_value(method, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext),

    BinTokens = convert_tokens(Tokens),
    put(<<"BOSS_INTERNAL_REQUEST_OBJECT">>, Req),
    put(<<"BOSS_INTERNAL_SESSION_ID">>, convert_session_id(SessionID)),
    Result = case proplists:get_value("handle_request", ExportStrings) of
        1 -> Module:handle_request(lists:keyreplace(tokens, 1, RequestContext, {tokens, BinTokens}));
        _ -> 
            case proplists:get_value(Action, ExportStrings) of
                Arity when Arity >= 2, Arity =< 5 ->
                    ActionAtom = list_to_atom(Action),
                    case Arity of
                        2 -> Module:ActionAtom(RequestMethod, BinTokens);
                        3 -> Module:ActionAtom(RequestMethod, BinTokens, AuthInfo);
                        4 -> Module:ActionAtom(Req, SessionID, RequestMethod, BinTokens);
                        5 -> Module:ActionAtom(Req, SessionID, RequestMethod, BinTokens, AuthInfo)
                    end;
                _ -> undefined
            end
    end,
    put(<<"BOSS_INTERNAL_REQUEST_OBJECT">>, undefined),
    put(<<"BOSS_INTERNAL_SESSION_ID">>, undefined),
    Result.

filter_config({Module, ExportStrings}, 'cache', Default, RequestContext) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    Tokens = proplists:get_value(tokens, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    BinTokens = convert_tokens(Tokens),
    case proplists:get_value("cache_", ExportStrings) of
        4 -> Module:cache_(Req, SessionID, Action, BinTokens);
        5 -> Module:cache_(Req, SessionID, Action, BinTokens, AuthInfo);
        _ -> filter_config1({Module, ExportStrings}, 'cache', Default, RequestContext)
    end;
filter_config({Module, ExportStrings}, 'lang', Default, RequestContext) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value("lang_", ExportStrings) of
        3 -> Module:lang_(Req, SessionID, Action);
        4 -> Module:lang_(Req, SessionID, Action, AuthInfo);
        _ -> filter_config1({Module, ExportStrings}, 'lang', Default, RequestContext)
    end;
filter_config(Info, FilterKey, Default, RequestContext) ->
    filter_config1(Info, FilterKey, Default, RequestContext).

filter_config1({Module, ExportStrings}, FilterKey, Default, RequestContext) ->
    case proplists:get_value("config", ExportStrings) of
        3 -> Module:config(FilterKey, Default, RequestContext);
        _ -> Default
    end.
