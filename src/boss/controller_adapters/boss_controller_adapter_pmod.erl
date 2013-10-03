% Either a great idea or a horrible one
-module(boss_controller_adapter_pmod).
-compile(export_all).

accept(Application, Controller, ControllerList) ->
    Module = boss_compiler_adapter_erlang:controller_module(Application, Controller),
    lists:member(Module, ControllerList).

wants_session(Application, Controller, ControllerList) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    lists:member({'new', 2}, Module:module_info(exports)).

init(Application, Controller, ControllerList, RequestContext) ->
    Req = proplists:get_value(request, RequestContext),
    SessionID = proplists:get_value(session_id, RequestContext),
    Module = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    ExportStrings = lists:map(
        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
        Module:module_info(exports)),
    ControllerInstance = case proplists:get_value("new", ExportStrings) of
        1 -> Module:new(Req);
        2 -> Module:new(Req, SessionID)
    end,
    {ControllerInstance, ExportStrings}.

filters(Type, {ControllerInstance, ExportStrings}, RequestContext, GlobalFilters) ->
    FunctionString = lists:concat([Type, "_filters"]),
    case proplists:get_value(FunctionString, ExportStrings) of
        3 -> 
            FunctionAtom = list_to_atom(FunctionString),
            ControllerInstance:FunctionAtom(GlobalFilters, RequestContext);
        _ -> GlobalFilters
    end.

before_filter({ControllerInstance, ExportStrings}, RequestContext) ->
    Action = proplists:get_value(action, RequestContext),
    RequestMethod = proplists:get_value(method, RequestContext),
    Tokens = proplists:get_value(tokens, RequestContext),

    AuthResult = case proplists:get_value("before_", ExportStrings) of
        2 -> ControllerInstance:before_(Action);
        4 -> ControllerInstance:before_(Action, RequestMethod, Tokens);
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

after_filter({ControllerInstance, ExportStrings}, RequestContext, Result) ->
    Action = proplists:get_value(action, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value("after_", ExportStrings) of
        3 -> ControllerInstance:after_(Action, Result);
        4 -> ControllerInstance:after_(Action, Result, AuthInfo);
        _ -> Result
    end.

action({ControllerInstance, ExportStrings}, RequestContext) ->
    Action = proplists:get_value(action, RequestContext),
    RequestMethod = proplists:get_value(method, RequestContext),
    Tokens = proplists:get_value(tokens, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value(Action, ExportStrings) of
        3 ->
            ActionAtom = list_to_atom(Action),
            ControllerInstance:ActionAtom(RequestMethod, Tokens);
        4 ->
            ActionAtom = list_to_atom(Action),
            ControllerInstance:ActionAtom(RequestMethod, Tokens, AuthInfo);
        _ -> undefined
    end.

filter_config({ControllerInstance, ExportStrings}, 'cache', Default, RequestContext) ->
    Action = proplists:get_value(action, RequestContext),
    Tokens = proplists:get_value(tokens, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value("cache_", ExportStrings) of
        3 -> ControllerInstance:cache_(Action, Tokens);
        4 -> ControllerInstance:cache_(Action, Tokens, AuthInfo);
        _ -> filter_config1({ControllerInstance, ExportStrings}, 'cache', Default, RequestContext)
    end;
filter_config({ControllerInstance, ExportStrings}, 'lang', Default, RequestContext) ->
    Action = proplists:get_value(action, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value("lang_", ExportStrings) of
        2 -> ControllerInstance:lang_(Action);
        3 -> ControllerInstance:lang_(Action, AuthInfo);
        _ -> filter_config1({ControllerInstance, ExportStrings}, 'lang', Default, RequestContext)
    end;
filter_config(Info, FilterModule, Default, RequestContext) ->
    filter_config1(Info, FilterModule, Default, RequestContext).

filter_config1({ControllerInstance, ExportStrings}, FilterKey, Default, RequestContext) ->
    case proplists:get_value("config", ExportStrings) of
        4 -> ControllerInstance:config(FilterKey, Default, RequestContext);
        _ -> Default
    end.
