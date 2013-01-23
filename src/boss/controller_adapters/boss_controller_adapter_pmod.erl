% Either a great idea or a horrible one
-module(boss_controller_adapter_pmod).
-compile(export_all).

accept(Application, Controller) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller)),
    code:which(Module) =/= non_existing.

init(Application, Controller, Req, SessionID) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller)),
    ExportStrings = lists:map(
        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
        Module:module_info(exports)),
    ControllerInstance = case proplists:get_value("new", ExportStrings) of
        1 -> Module:new(Req);
        2 -> Module:new(Req, SessionID)
    end,
    {ControllerInstance, ExportStrings}.

before_filter({ControllerInstance, ExportStrings}, Action, RequestMethod, Tokens) ->
    case proplists:get_value("before_", ExportStrings) of
        2 -> ControllerInstance:before_(Action);
        4 -> ControllerInstance:before_(Action, RequestMethod, Tokens);
        _ -> ok
    end.

action({ControllerInstance, ExportStrings}, Action, RequestMethod, Tokens, AuthInfo) ->
    case proplists:get_value(Action, ExportStrings) of
        3 ->
            ActionAtom = list_to_atom(Action),
            ControllerInstance:ActionAtom(RequestMethod, Tokens);
        4 ->
            ActionAtom = list_to_atom(Action),
            ControllerInstance:ActionAtom(RequestMethod, Tokens, AuthInfo);
        _ -> undefined
    end.

language({ControllerInstance, ExportStrings}, Action, AuthInfo) ->
    case proplists:get_value("lang_", ExportStrings) of
        2 -> ControllerInstance:lang_(Action);
        3 -> ControllerInstance:lang_(Action, AuthInfo);
        _ -> auto
    end.

after_filter({ControllerInstance, ExportStrings}, Action, Result, AuthInfo) ->
    case proplists:get_value("after_", ExportStrings) of
        3 -> ControllerInstance:after_(Action, Result);
        4 -> ControllerInstance:after_(Action, Result, AuthInfo);
        _ -> Result
    end.
