-module(boss_lang).
-compile(export_all).

create_new(NewLang) ->
    LangStringsFile = boss_files:lang_strings_path(NewLang),
    {ok, IODevice} = file:open(LangStringsFile, [write]),
    file:close(IODevice),
    LangBlocksFile = boss_files:lang_blocks_path(NewLang),
    {ok, IODevice2} = file:open(LangBlocksFile, [write]),
    file:close(IODevice2).	
	
generate_po_file(string, Mode, Lang, Messages) ->
	IODevice = open_po_strings_file(Lang),
    lists:map(fun(Message) ->
                Original = proplists:get_value("orig", Message),
                Translation = proplists:get_value("trans", Message),
                case Translation of
                    "" -> 
						case Mode of
							full -> write_po_line_to_file(IODevice, Original, Translation);
							_ -> ok
						end;
                    _ -> write_po_line_to_file(IODevice, Original, Translation)
                end
        end, Messages),
	close_po_file(IODevice);
generate_po_file(block, Mode, Lang, Messages) ->
	IODevice = open_po_blocks_file(Lang),
    lists:map(fun(Message) ->
                Original = proplists:get_value("orig", Message),
                Translation = proplists:get_value("trans", Message),
                case Translation of
                    "" -> 
						case Mode of
							full -> write_po_line_to_file(IODevice, Original, Translation);
							_ -> ok
						end;
                    _ -> write_po_line_to_file(IODevice, Original, Translation)
                end
        end, Messages),
	close_po_file(IODevice).

open_po_strings_file(Lang) ->
    LangFile = boss_files:lang_strings_path(Lang),
    {ok, IODevice} = file:open(LangFile, [write, append]),
	IODevice.

open_po_blocks_file(Lang) ->
    LangFile = boss_files:lang_blocks_path(Lang),
    {ok, IODevice} = file:open(LangFile, [write, append]),
	IODevice.

close_po_file(IODevice) ->
	file:close(IODevice).

write_po_line_to_file(IODevice, Original, Translation) ->
	OriginalEncoded = unicode:characters_to_list(boss_lang:escape_quotes(Original)),
	TranslationEncoded = unicode:characters_to_list(boss_lang:escape_quotes(Translation)),
    file:write(IODevice, io_lib:format("\nmsgid \"~ts\"\n",[list_to_binary(OriginalEncoded)])),	   
	file:write(IODevice, io_lib:format("\msgstr \"~ts\"\n",[list_to_binary(TranslationEncoded)])).

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

extract_blocks(Lang) ->
    AllBlocks = extract_view_blocks(),
    PoBlocks = extract_po_blocks(Lang),
    UntranslatedBlocks = lists:filter(fun(S) -> 
                case proplists:get_value(S, PoBlocks) of
                    undefined -> true;
                    _ -> false
                end
        end, lists:usort(AllBlocks)),
    {UntranslatedBlocks, PoBlocks}.

extract_po_strings(Lang) ->
    LangFile = boss_files:lang_strings_path(Lang),
    Tokens = po_scanner:scan(LangFile),
    process_po_tokens(Tokens, []).

extract_po_blocks(Lang) ->
    LangFile = boss_files:lang_blocks_path(Lang),
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

extract_view_blocks() ->
    ViewFiles = boss_files:view_file_list(),
    lists:foldl(fun(File, Acc) -> Acc ++ process_view_file_blocks(File) end,
            [], ViewFiles).

process_view_file_blocks(ViewFile) ->
    {ok, Contents} = file:read_file(ViewFile),
    {ok, Tokens} = erlydtl_scanner:scan(unicode:characters_to_list(Contents)),
    process_view_file_tokens_blocks(Tokens, []).

process_view_file_tokens_blocks([], Acc) ->
    Acc;
process_view_file_tokens_blocks([{blocktrans_keyword, _, _}, {identifier, _, Identifier}|Rest], Acc) ->
    process_view_file_tokens_blocks(Rest, [atom_to_list(Identifier)|Acc]);
process_view_file_tokens_blocks([_|Rest], Acc) ->
    process_view_file_tokens_blocks(Rest, Acc).

extract_view_strings() ->
    ViewFiles = boss_files:view_file_list(),
    lists:foldl(fun(File, Acc) -> Acc ++ process_view_file(File) end,
            [], ViewFiles).

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
