-module(boss_lang).
-export([create_lang/2, 
        delete_lang/2, 
        extract_strings/1,
        extract_strings/2,
        extract_po_strings/2,
        extract_po_blocks/3,
        escape_quotes/1,
        update_po/1,
        update_po/4]).

create_lang(App, Lang) ->
    LangFile = boss_files:lang_path(App, Lang),
    {ok, IODevice} = file:open(LangFile, [write]),
    file:close(IODevice).

delete_lang(App, Lang) ->
    ok = file:delete(boss_files:lang_path(App, Lang)).

%% @doc Update the po file with all unstranslated messages for all languages (filled with blanks)
update_po(App) ->
    lists:map(fun(X) -> update_po(App, X, all, []) end, boss_files:language_list(App)).

%% @doc Update the po file with all untranslated messages for the given language (filled with blanks)
%% @spec update_po( Lang::string(), all::atom(), [] ) -> ok | {error, Reason}
update_po(App, Lang, all, []) ->
	{Untranslated, _Translated} = boss_lang:extract_strings(App, Lang),
	case Untranslated of
		[] -> ok;
		_ ->
			Translations = lists:map(fun(X) -> [{"orig", X}, {"trans", ""}] end, Untranslated),
			update_po(App, Lang, all, Translations)
	end;

%% @doc Update the po file for a given Language
%% @spec update_po( Lang::string(), Mode::atom(all|filled), Translation::TupleList([{"orig", "x"}, {"trans", "y"}]) ) -> ok | {error, Reason}
update_po(App, Lang, Mode, Translations) ->
    LangFile = boss_files:lang_path(App, Lang),
    {ok, IODevice} = file:open(LangFile, [write, append]),	
    lists:map(fun(Message) ->
                Original = proplists:get_value("orig", Message),
                Translation = proplists:get_value("trans", Message),
				BlockIdentifier = proplists:get_value("identifier", Message),
                case Translation of
                    "" -> 
						case Mode of
							filled -> ok;
							all -> lang_write_to_file(IODevice, Original, Translation, BlockIdentifier)
						end;
                    _ -> lang_write_to_file(IODevice, Original, Translation, BlockIdentifier)
                end
        end, Translations),
	file:close(IODevice).

lang_write_to_file(IODevice, Original, Translation, BlockIdentifier) ->
	OriginalEncoded = boss_lang:escape_quotes(Original),
	TranslationEncoded = boss_lang:escape_quotes(Translation),
	case BlockIdentifier of
		undefined -> 
			file:write(IODevice, io_lib:format("\nmsgid \"~ts\"\n",[OriginalEncoded])),
			file:write(IODevice, io_lib:format("\msgstr \"~ts\"\n",[TranslationEncoded]));
		Identifier -> 
			file:write(IODevice, io_lib:format("\n#. ~ts\n",[Identifier])),
			file:write(IODevice, io_lib:format("msgid \"~s\"\n", [""])),
                        OriginalTokens = re:split(OriginalEncoded,"\r\n", [{return, list}]),
			lang_write_multiline_to_file(IODevice, OriginalTokens),
			file:write(IODevice, io_lib:format("\msgstr \"~s\"\n", [""])),
                        TranslationTokens = re:split(TranslationEncoded,"\r\n", [{return, list}]),
			lang_write_multiline_to_file(IODevice, TranslationTokens)
	end.

lang_write_multiline_to_file(_IODevice, []) -> ok;
lang_write_multiline_to_file(IODevice, [Token|Rest]) ->
	ParsedToken = case Token of
		[] -> "";
		_ -> Token
	end,
	case Rest of
		[] -> file:write(IODevice, io_lib:format("\"~ts\"\n", [ParsedToken]));
		_ -> file:write(IODevice, io_lib:format("\"~ts~c~c\"\n", [ParsedToken, 92, 110]))
	end,
	lang_write_multiline_to_file(IODevice, Rest).

extract_strings(App) ->
    lists:usort(extract_model_strings(App) ++ extract_view_strings(App)).

extract_strings(App, Lang) ->
    AllStrings = extract_strings(App),
    PoStrings = extract_po_strings(App, Lang) ++ extract_po_blocks(App, Lang, id),
    UntranslatedStrings = lists:filter(fun(S) -> 
                ToCheck = case S of
                    [{identifier, _Identifier}, {string, String}] -> 
                        binary_to_list(String);
                    _ -> S
                end,
                case proplists:get_value(ToCheck, PoStrings) of
                    undefined -> true;
                    _ -> false
                end
        end, lists:usort(AllStrings)),
    {UntranslatedStrings, PoStrings}.

extract_po_strings(App, Lang) ->
    LangFile = boss_files:lang_path(App, Lang),
    Tokens = po_scanner:scan(LangFile),
    process_po_tokens(Tokens, []).

extract_po_blocks(App, Lang, Mode) ->
    LangFile = boss_files:lang_path(App, Lang),
    Tokens = po_scanner:scan(LangFile),
    process_po_block_tokens(Tokens, Mode, []).

process_po_tokens([], Acc) ->
    lists:reverse(Acc);
process_po_tokens([{comment, _MsgComment}, {id, _MsgId}, {str, _MsgStr}|Rest], Acc) ->
	process_po_tokens(Rest, Acc);
process_po_tokens([{id, MsgId}, {str, MsgStr}|Rest], Acc) ->
    process_po_tokens(Rest, [{MsgId, MsgStr}|Acc]);
process_po_tokens([_|Rest], Acc) ->
    process_po_tokens(Rest, Acc).

process_po_block_tokens([], _Mode, Acc) ->
    lists:reverse(Acc);
process_po_block_tokens([{id, _MsgId}, {str, _MsgStr}|Rest], Mode, Acc) ->
	process_po_block_tokens(Rest, Mode, Acc);	
process_po_block_tokens([{comment, MsgComment}, {id, MsgId}, {str, MsgStr}|Rest], Mode, Acc) ->
	Id = case Mode of
			 id -> MsgId;
			 comment -> string:substr(MsgComment, 3, string:len(MsgComment) - 2)
		 end,
    process_po_block_tokens(Rest, Mode, [{Id, MsgStr}|Acc]);
process_po_block_tokens([_|Rest], Mode, Acc) ->
    process_po_block_tokens(Rest, Mode, Acc).

extract_model_strings(App) ->
    lists:foldl(fun(Type, Acc) ->
                TypeAtom = list_to_atom(Type),
                Exports = TypeAtom:module_info(exports),
                case lists:member({validation_tests, 1}, Exports) of
                    true ->
                        DummyRecord = boss_record_lib:dummy_record(TypeAtom),
                        Messages = lists:map(fun({_TestFun, TestMsg}) -> TestMsg end, 
                            DummyRecord:validation_tests()),
                        Messages ++ Acc;
                    false ->
                        Acc
                end
        end, [], boss_files:model_list(App)).

extract_view_strings(App) ->
    case boss_env:is_developing_app(App) of
        true ->
            ViewFiles = boss_files:view_file_list(),
            lists:foldl(fun(File, Acc) -> Acc ++ process_view_file(File) ++ process_view_file_blocks(File) end,
                [], ViewFiles);
        false ->
            lists:foldl(
                fun(Module, Acc) ->
                        Module:translatable_strings() ++ Acc
                end, [], boss_env:get_env(App, view_modules, []) ++ boss_env:get_env(App, view_lib_modules, []))
    end.

process_view_file_blocks(ViewFile) ->
    {ok, Contents} = file:read_file(ViewFile),
    {ok, Tokens} = blocktrans_scanner:scan(binary_to_list(Contents)),
    lists:map(fun(X) -> [{identifier, element(1, X)}, {string, element(2, X)}] end, blocktrans_parser:parse(Tokens)).

process_view_file(ViewFile) ->
    {ok, Contents} = file:read_file(ViewFile),
    {ok, Tokens} = erlydtl_scanner:scan(binary_to_list(Contents)),
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
