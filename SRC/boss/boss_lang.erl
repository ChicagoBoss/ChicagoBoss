-module(boss_lang).
-compile(export_all).

extract_strings() ->
    lists:usort(extract_model_strings() ++ extract_view_strings()).

extract_strings(Lang) ->
    AllStrings = extract_strings(),
    PoStrings = extract_po_strings(Lang),
    UntranslatedStrings = lists:filter(fun(S) -> 
                case proplists:get_value(S, PoStrings) of
                    undefined -> true;
                    _ -> false
                end
        end, lists:usort(AllStrings)),
    {UntranslatedStrings, PoStrings}.

extract_po_strings(Lang) ->
    LangFile = boss_files:lang_path(Lang),
    Tokens = po_scanner:scan(LangFile),
    process_po_tokens(Tokens, []).

process_po_tokens([], Acc) ->
    lists:reverse(Acc);
process_po_tokens([{id, MsgId}, {str, MsgStr}|Rest], Acc) ->
    process_po_tokens(Rest, [{MsgId, MsgStr}|Acc]);
process_po_tokens([_|Rest], Acc) ->
    process_po_tokens(Rest, Acc).

extract_model_strings() ->
    lists:foldl(fun(Model, Acc) ->
                Type = list_to_atom(Model),
                Exports = Type:module_info(exports),
                case lists:member({validation_tests, 1}, Exports) of
                    true ->
                        NumParams = proplists:get_value(new, Exports),
                        DummyRecord = apply(Type, new, lists:seq(1, NumParams)),
                        Messages = lists:map(fun({_TestFun, TestMsg}) -> TestMsg end, 
                            DummyRecord:validation_tests()),
                        Messages ++ Acc;
                    false ->
                        Acc
                end
        end, [], boss_files:model_list()).

extract_view_strings() ->
    ViewFiles = boss_files:view_file_list(),
    lists:foldl(fun(File, Acc) -> Acc ++ process_view_file(File) end,
            [], ViewFiles).

process_view_file(ViewFile) ->
    {ok, Contents} = file:read_file(ViewFile),
    {ok, Tokens} = erlydtl_scanner:scan(binary_to_list(Contents)),
    process_view_file_tokens(Tokens, []).

process_view_file_tokens([], Acc) ->
    Acc;
process_view_file_tokens([{trans_keyword, _, _}, {string_literal, _, String}|Rest], Acc) ->
    process_view_file_tokens(Rest, 
        [unescape_string_literal(string:strip(String, both, $"))|Acc]);
process_view_file_tokens([_|Rest], Acc) ->
    process_view_file_tokens(Rest, Acc).

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).

escape_quotes(String) ->
    escape_quotes(String, []).

escape_quotes([], Acc) ->
    lists:reverse(Acc);
escape_quotes([$"|Rest], Acc) ->
    escape_quotes(Rest, [$", $\\|Acc]);
escape_quotes([H|T], Acc) ->
    escape_quotes(T, [H|Acc]).
