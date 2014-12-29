% Either a great idea or a horrible one
-module(boss_controller_adapter_pmod).
-compile(export_all).


-spec get_instance({atom() | tuple(),[any()]},[any()]) -> any().
-spec accept(_,_,[any()]) -> boolean().
-spec wants_session(_,_,_) -> boolean().
-spec init(types:application(),types:controller(),[types:controller()],_) -> 
    {module(),[{_,_}]}.
-spec filters(atom() | string() | number(),{atom() | tuple(),[any()]},[any()],_) -> any().
-spec before_filter({atom() | tuple(),[any()]},[any()]) -> any().
-spec after_filter({atom() | tuple(),[any()]},[any()],_) -> any().
-spec action({atom() | tuple(),[any()]},[any()]) -> any().
-spec filter_config({atom() | tuple(),[any()]},_,_,[any()]) -> any().
-spec filter_config1({atom() | tuple(),[any()]},_,_,[any()]) -> any().

get_instance({ControllerModule, ExportStrings}, RequestContext) ->
    Req		= proplists:get_value(request, RequestContext),
    SessionID	= proplists:get_value(session_id, RequestContext),
    case proplists:get_value("new", ExportStrings) of
        1 -> ControllerModule:new(Req);
        2 -> ControllerModule:new(Req, SessionID)
    end.

accept(Application, Controller, ControllerList) ->
    Module = boss_compiler_adapter_erlang:controller_module(Application, Controller),
    lists:member(Module, ControllerList).

wants_session(Application, Controller, ControllerList) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    lists:member({'new', 2}, Module:module_info(exports)).

init(Application, Controller, ControllerList, _RequestContext) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    ExportStrings = lists:map(
        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
        Module:module_info(exports)),
    {Module, ExportStrings}.

filters(Type, {_, ExportStrings} = Info, RequestContext, GlobalFilters) ->
    ControllerInstance = get_instance(Info, RequestContext),
    FunctionString = lists:concat([Type, "_filters"]),
    case proplists:get_value(FunctionString, ExportStrings) of
        3 -> 
            FunctionAtom = list_to_atom(FunctionString),
            ControllerInstance:FunctionAtom(GlobalFilters, RequestContext);
        _ -> GlobalFilters
    end.

before_filter({_, ExportStrings} = Info, RequestContext) ->
    ControllerInstance	= get_instance(Info, RequestContext),
    Action		= proplists:get_value(action, RequestContext),
    RequestMethod	= proplists:get_value(method, RequestContext),
    Tokens		= proplists:get_value(tokens, RequestContext),

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
        {ok, AuthInfo} ->
            {ok, [{'_before', AuthInfo}|RequestContext]};
        Other ->
            Other
    end.

after_filter({_, ExportStrings} = Info, RequestContext, Result) ->
    ControllerInstance = get_instance(Info, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value("after_", ExportStrings) of
        3 -> ControllerInstance:after_(Action, Result);
        4 -> ControllerInstance:after_(Action, Result, AuthInfo);
        _ -> Result
    end.

action({_, ExportStrings} = Info, RequestContext) ->
    ControllerInstance	= get_instance(Info, RequestContext),
    Action		= proplists:get_value(action, RequestContext),
    RequestMethod	= proplists:get_value(method, RequestContext),
    Tokens		= proplists:get_value(tokens, RequestContext),
    AuthInfo		= proplists:get_value('_before', RequestContext, RequestContext),
    ActionAtom          = list_to_atom(Action),
    lager:notice("Request Method: ~p, Tokens: ~p", [RequestMethod, Tokens]),
    case proplists:get_value(Action, ExportStrings) of
        3 ->
            ControllerInstance:ActionAtom(RequestMethod, Tokens);
        4 ->
            ControllerInstance:ActionAtom(RequestMethod, Tokens, AuthInfo);
        _ ->
	    CMod = element(1, ControllerInstance),
	    lager:notice("[ChicagoBoss] The function ~p:~s/2 is not exported, "++
			 "if in doubt add -export([~s/2])) to the module",
			 [CMod, Action, Action]),
	    undefined
    end.

filter_config({_, ExportStrings} = Info, 'cache', Default, RequestContext) ->
    ControllerInstance = get_instance(Info, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    Tokens = proplists:get_value(tokens, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value("cache_", ExportStrings) of
        3 -> ControllerInstance:cache_(Action, Tokens);
        4 -> ControllerInstance:cache_(Action, Tokens, AuthInfo);
        _ -> filter_config1(Info, 'cache', Default, RequestContext)
    end;
filter_config({_, ExportStrings} = Info, 'lang', Default, RequestContext) ->
    ControllerInstance = get_instance(Info, RequestContext),
    Action = proplists:get_value(action, RequestContext),
    AuthInfo = proplists:get_value('_before', RequestContext, RequestContext),

    case proplists:get_value("lang_", ExportStrings) of
        2 -> ControllerInstance:lang_(Action);
        3 -> ControllerInstance:lang_(Action, AuthInfo);
        _ -> filter_config1(Info, 'lang', Default, RequestContext)
    end;
filter_config(Info, FilterModule, Default, RequestContext) ->
    filter_config1(Info, FilterModule, Default, RequestContext).

filter_config1({_, ExportStrings} = Info, FilterKey, Default, RequestContext) ->
    ControllerInstance = get_instance(Info, RequestContext),
    case proplists:get_value("config", ExportStrings) of
        4 -> ControllerInstance:config(FilterKey, Default, RequestContext);
        _ -> Default
    end.
