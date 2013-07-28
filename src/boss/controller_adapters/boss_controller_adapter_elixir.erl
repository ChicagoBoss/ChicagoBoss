-module(boss_controller_adapter_elixir).
-compile(export_all).

convert_tokens(Tokens) -> lists:map(fun list_to_binary/1, Tokens).
convert_session_id(undefined) -> undefined;
convert_session_id(SessionID) -> list_to_binary(SessionID).

accept(Application, Controller, ControllerList) ->
    Module = boss_compiler_adapter_elixir:controller_module(Application, Controller),
    lists:member(Module, ControllerList).

wants_session(_Application, _Controller, _ControllerList) ->
    true.

init(Application, Controller, ControllerList, Req, SessionID) ->
    Module = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    ExportStrings = lists:map(
        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
        Module:module_info(exports)),
    {Module, ExportStrings, Req, SessionID}.

before_filter({Module, ExportStrings, Req, SessionID}, Action, RequestMethod, Tokens) ->
    BinTokens = convert_tokens(Tokens),
    case proplists:get_value("before_", ExportStrings) of
        3 -> Module:before_(Req, SessionID, Action);
        5 -> Module:before_(Req, SessionID, Action, RequestMethod, BinTokens);
        _ -> ok
    end.

cache_info({Module, ExportStrings, Req, SessionID}, Action, Tokens, AuthInfo) ->
    BinTokens = convert_tokens(Tokens),
    case proplists:get_value("cache_", ExportStrings) of
        4 -> Module:cache_(Req, SessionID, Action, BinTokens);
        5 -> Module:cache_(Req, SessionID, Action, BinTokens, AuthInfo);
        _ -> ok
    end.

action({Module, ExportStrings, Req, SessionID}, Action, RequestMethod, Tokens, AuthInfo) ->
    BinTokens = convert_tokens(Tokens),
    put(<<"BOSS_INTERNAL_REQUEST_OBJECT">>, Req),
    put(<<"BOSS_INTERNAL_SESSION_ID">>, convert_session_id(SessionID)),
    Result = case proplists:get_value(Action, ExportStrings) of
        Arity when Arity >= 2, Arity =< 5 ->
            ActionAtom = list_to_atom(Action),
            case Arity of
                2 -> Module:ActionAtom(RequestMethod, BinTokens);
                3 -> Module:ActionAtom(RequestMethod, BinTokens, AuthInfo);
                4 -> Module:ActionAtom(Req, SessionID, RequestMethod, BinTokens);
                5 -> Module:ActionAtom(Req, SessionID, RequestMethod, BinTokens, AuthInfo)
            end;
        _ -> undefined
    end,
    put(<<"BOSS_INTERNAL_REQUEST_OBJECT">>, undefined),
    put(<<"BOSS_INTERNAL_SESSION_ID">>, undefined),
    Result.

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
