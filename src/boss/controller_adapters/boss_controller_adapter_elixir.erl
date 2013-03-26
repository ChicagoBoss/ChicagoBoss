
-module(boss_controller_adapter_elixir).
-compile(export_all).

accept(Application, Controller, ControllerList) ->
    Module = boss_compiler_adapter_elixir:controller_module(Application, Controller),
    lists:member(Module, ControllerList).

init(Application, Controller, ControllerList, Req, SessionID) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    ExportStrings = lists:map(
        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
        Module:module_info(exports)),
    {Module, ExportStrings, Req, SessionID}.

before_filter({Module, ExportStrings, Req, SessionID}, Action, RequestMethod, Tokens) ->
    BinTokens = lists:map(fun list_to_binary/1, Tokens),
    case proplists:get_value("before_", ExportStrings) of
        3 -> Module:before_(Req, SessionID, Action);
        5 -> Module:before_(Req, SessionID, Action, RequestMethod, BinTokens);
        _ -> ok
    end.

cache_info({Module, ExportStrings, Req, SessionID}, Action, Tokens, AuthInfo) ->
    BinTokens = lists:map(fun list_to_binary/1, Tokens),
    case proplists:get_value("cache_", ExportStrings) of
        4 -> Module:cache_(Req, SessionID, Action, BinTokens);
        5 -> Module:cache_(Req, SessionID, Action, BinTokens, AuthInfo);
        _ -> ok
    end.

action({Module, ExportStrings, Req, SessionID}, Action, RequestMethod, Tokens, AuthInfo) ->
    BinTokens = lists:map(fun list_to_binary/1, Tokens),
    case proplists:get_value(Action, ExportStrings) of
        4 ->
            ActionAtom = list_to_atom(Action),
            Module:ActionAtom(Req, SessionID, RequestMethod, BinTokens);
        5 ->
            ActionAtom = list_to_atom(Action),
            Module:ActionAtom(Req, SessionID, RequestMethod, BinTokens, AuthInfo);
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
