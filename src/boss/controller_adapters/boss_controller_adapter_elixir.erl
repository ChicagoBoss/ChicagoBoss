
-module(boss_controller_adapter_elixir).
-compile(export_all).

accept(Application, Controller) ->
    Module = list_to_atom(boss_files:web_controller_ex(Application, Controller)),
    code:which(Module) =/= non_existing.

init(Application, Controller, Req, SessionID) ->
    Module = list_to_atom(boss_files:web_controller_ex(Application, Controller)),
    ExportStrings = lists:map(
        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
        Module:module_info(exports)),
    {Module, ExportStrings, Req, SessionID}.

before_filter({Module, ExportStrings, Req, SessionID}, Action, RequestMethod, Tokens) ->
    case proplists:get_value("before_", ExportStrings) of
        3 -> Module:before_(Req, SessionID, Action);
        5 -> Module:before_(Req, SessionID, Action, RequestMethod, Tokens);
        _ -> ok
    end.

action({Module, ExportStrings, Req, SessionID}, Action, RequestMethod, Tokens, AuthInfo) ->
    case proplists:get_value(Action, ExportStrings) of
        4 ->
            ActionAtom = list_to_atom(Action),
            Module:ActionAtom(Req, SessionID, RequestMethod, Tokens);
        5 ->
            ActionAtom = list_to_atom(Action),
            Module:ActionAtom(Req, SessionID, RequestMethod, Tokens, AuthInfo);
        _ -> undefined
    end.

language({Module, ExportStrings, Req, SessionID}, Action, AuthInfo) ->
    case proplists:get_value("lang_", ExportStrings) of
        3 -> Module:lang_(Req, SessionID, Action);
        4 -> Module:lang_(Req, SessionID, Action, AuthInfo);
        _ -> auto
    end.

after_filter({Module, ExportStrings, Req, SessionID}, Action, Result, AuthInfo) ->
    case proplists:get_value("after_", ExportStrings) of
        4 -> Module:after_(Req, SessionID, Action, Result);
        5 -> Module:after_(Req, SessionID, Action, Result, AuthInfo);
        _ -> Result
    end.
