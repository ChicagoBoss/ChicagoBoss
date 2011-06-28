-module(boss_lang).
-compile(export_all).

create_lang(Lang) ->
    LangFile = boss_files:lang_path(Lang),
    {ok, IODevice} = file:open(LangFile, [write]),
    file:close(IODevice).

delete_lang(Lang) ->
	ok = file:delete(boss_files:lang_path(Lang)).

extract_strings() ->
    lists:usort(extract_model_strings() ++ extract_view_strings()).

extract_strings(Lang) ->
    AllStrings = extract_strings(),
    PoStrings = extract_po_strings(Lang) ++ extract_po_blocks(Lang, id),
    UntranslatedStrings = lists:filter(fun(S) -> 
				ToCheck = case S of
					[{identifier, Identifier}, {string, String}] -> 
						binary_to_list(String);
					_ -> S
				end,
                case proplists:get_value(ToCheck, PoStrings) of
                    undefined -> true;
                    _ -> false
                end
        end, lists:usort(AllStrings)),
    {UntranslatedStrings, PoStrings}.

extract_po_strings(Lang) ->
    LangFile = boss_files:lang_path(Lang),
    Tokens = po_scanner:scan(LangFile),
    process_po_tokens(Tokens, []).

extract_po_blocks(Lang, Mode) ->
    LangFile = boss_files:lang_path(Lang),
    Tokens = po_scanner:scan(LangFile),
    process_po_block_tokens(Tokens, Mode, []).

process_po_tokens([], Acc) ->
    lists:reverse(Acc);
process_po_tokens([{comment, MsgComment}, {id, MsgId}, {str, MsgStr}|Rest], Acc) ->
	process_po_tokens(Rest, Acc);
process_po_tokens([{id, MsgId}, {str, MsgStr}|Rest], Acc) ->
    process_po_tokens(Rest, [{MsgId, MsgStr}|Acc]);
process_po_tokens([_|Rest], Acc) ->
    process_po_tokens(Rest, Acc).

process_po_block_tokens([], Mode, Acc) ->
    lists:reverse(Acc);
process_po_block_tokens([{id, MsgId}, {str, MsgStr}|Rest], Mode, Acc) ->
	process_po_block_tokens(Rest, Mode, Acc);	
process_po_block_tokens([{comment, MsgComment}, {id, MsgId}, {str, MsgStr}|Rest], Mode, Acc) ->
	Id = case Mode of
			 id -> MsgId;
			 comment -> string:substr(MsgComment, 3, string:len(MsgComment) - 2)
		 end,
    process_po_block_tokens(Rest, Mode, [{Id, MsgStr}|Acc]);
process_po_block_tokens([_|Rest], Mode, Acc) ->
    process_po_block_tokens(Rest, Mode, Acc).

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
    lists:foldl(fun(File, Acc) -> Acc ++ process_view_file(File) ++ process_view_file_blocks(File) end,
            [], ViewFiles).

process_view_file_blocks(ViewFile) ->
    {ok, Contents} = file:read_file(ViewFile),
    {ok, Tokens} = blocktrans_scanner:scan(unicode:characters_to_list(Contents)),
	lists:map(fun(X) -> [{identifier, element(1, X)}, {string, element(2, X)}] end, blocktrans_parser:parse(Tokens)).

process_view_file(ViewFile) ->
    {ok, Contents} = file:read_file(ViewFile),
    {ok, Tokens} = erlydtl_scanner:scan(unicode:characters_to_list(Contents)),
    process_view_file_tokens(Tokens, []).

process_view_file_tokens([], Acc) ->
    Acc;
process_view_file_tokens([{trans_keyword, _, _}, {string_literal, _, String}|Rest], Acc) ->
    process_view_file_tokens(Rest, 
        [unescape_string_literal(string:strip(String, both, $"))|Acc]);
process_view_file_tokens([{'_', _}, {'(', _}, {string_literal, _, String}, {')', _}|Rest], Acc) ->
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
