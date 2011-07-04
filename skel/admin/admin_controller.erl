-module(admin_controller, [Req, SessionID]).
-compile(export_all).
-define(RECORDS_PER_PAGE, 100).

before_("access_denied") ->
    ok;
before_(_) ->
    ClientIp = case Req:header(x_forwarded_for) of
        undefined -> Req:peer_ip();
        IP -> IP
    end,
    Authorized = lists:foldr(fun
            (IPBlock, false) ->
                case string:tokens(IPBlock, "/") of
                    [IPAddress] ->
                        IPAddress =:= string:join(lists:map(fun erlang:integer_to_list/1, 
                                tuple_to_list(ClientIp)), ".");
                    [IPAddress, Mask] ->
                        MaskInt = list_to_integer(Mask),
                        IPAddressTuple = list_to_tuple(lists:map(fun erlang:list_to_integer/1, string:tokens(IPAddress, "."))),
                        admin_lib:mask_ipv4_address(ClientIp, MaskInt) =:= admin_lib:mask_ipv4_address(IPAddressTuple, MaskInt)
                end;
            (_, true) ->
                true
        end, false, boss_env:get_env(admin_ip_blocks, ["192.168.0.0/16", "127.0.0.1", "10.0.0.0/16"])),
    case Authorized of
        true ->
            {ok, local};
        _ ->
            {redirect, "/admin/access_denied"}
    end.

index('GET', [], Authorization) ->
	[{loaded, ModulesLoaded}, _, _, _, _, _] = application:info(),
	ConfigValues = [ [{Key, Value}] || {Key, Value} <- application:get_all_env()],
	SystemValues = [ {otp_release, erlang:system_info(system_version)},
            {processors, erlang:system_info(logical_processors_online)},
            {uptime, admin_lib:uptime()}
        ],
    {ok, [ {index_section, true}, {modules_loaded, ModulesLoaded}, {config_env, ConfigValues}, {system_env, SystemValues}] }.

routes('GET', [], Authorization) ->
    {ok, [ {routes_section, true}, {routes, boss_router:get_all()} ]};
routes('GET', ["reload"], Authorization) ->
    boss_router:reload(),
    boss_flash:add(SessionID, notice, "Routes reloaded"),
    {redirect, boss_router:base_url() ++ "/admin/routes"}.

heartbeat('POST', [WatchName], Authorization) ->
    boss_news:extend_watch(list_to_integer(WatchName)),
    {output, ""}.

watch('POST', [], Authorization) ->
    TopicString = Req:post_param("topic_string"),
    {ok, WatchId} = boss_news:watch(TopicString, fun admin_lib:push_update/3, "admin"++SessionID, 60), 
    {json, [{watch_id, WatchId}]}.

events('GET', [Since], Authorization) ->
    io:format("Pulling events...~n", []),
    {ok, Timestamp, Messages} = boss_mq:pull("admin" ++ SessionID, list_to_integer(Since), 30),
    {json, [{messages, Messages}, {timestamp, Timestamp}]}.

model('GET', [], Authorization) ->
    {ok, [{model_section, true}, {records, []}, {models, boss_files:model_list()}, {this_model, ""}, {topic_string, ""}]};
model('GET', [ModelName], Authorization) ->
    model('GET', [ModelName, "1"], Authorization);
model('GET', [ModelName, PageName], Authorization) ->
    Page = list_to_integer(PageName),
    Model = list_to_atom(ModelName),
    RecordCount = boss_db:count(Model),
    Records = boss_db:find(Model, [], ?RECORDS_PER_PAGE, (Page - 1) * ?RECORDS_PER_PAGE, id, str_descending),
    TopicString = string:join(lists:map(fun(Record) -> Record:id() ++ ".*" end, Records), ", "),
    AttributesWithDataTypes = lists:map(fun(Record) ->
                {Record:id(), lists:map(fun({Key, Val}) ->
                            {Key, Val, boss_db:data_type(Key, Val)}
                    end, Record:attributes())}
        end, Records),
    AttributeNames = case length(Records) of
        0 -> [];
        _ -> (lists:nth(1, Records)):attribute_names()
    end,
    Pages = lists:seq(1, ((RecordCount-1) div ?RECORDS_PER_PAGE)+1),
    {ok, 
        [{records, AttributesWithDataTypes}, {attribute_names, AttributeNames}, 
            {models, boss_files:model_list()}, {this_model, ModelName}, 
            {pages, Pages}, {this_page, Page}, {model_section, true},
            {topic_string, TopicString}, {timestamp, boss_mq:now("admin"++SessionID)}], 
        [{"Cache-Control", "no-cache"}]}.

csv('GET', [ModelName], Authorization) ->
    Model = list_to_atom(ModelName),
    [First|_] = Records = boss_db:find(Model, [], all, 0, id, str_descending),
    FirstLine = [lists:foldr(fun
                (Attr, []) ->
                    [atom_to_list(Attr)];
                (Attr, Acc) ->
                    [atom_to_list(Attr), ","|Acc]
            end, [], First:attribute_names()), "\n"],
    RecordLines = lists:map(fun(Record) ->
                [lists:foldr(fun
                            ({_Key, Val}, []) ->
                                [admin_lib:encode_csv_value(Val)];
                            ({_Key, Val}, Acc) ->
                                [admin_lib:encode_csv_value(Val), ","|Acc]
                        end, [], Record:attributes()), "\n"]
        end, Records),
    {output, [FirstLine, RecordLines], [{"Content-Type", "text/csv"}, 
            {"Content-Disposition", "attachment;filename="++ModelName++".csv"}]}.

upload('GET', [ModelName], Authorization) ->
    Module = list_to_atom(ModelName),
    NumArgs = proplists:get_value('new', Module:module_info(exports)),
    DummyRecord = apply(Module, 'new', lists:seq(1, NumArgs)),
    {ok, [{type, ModelName}, {attributes, DummyRecord:attribute_names()}]};
upload('POST', [ModelName], Authorization) ->
    Module = list_to_atom(ModelName),
    NumArgs = proplists:get_value('new', Module:module_info(exports)),
    DummyRecord = apply(Module, 'new', lists:map(fun(1) -> 'id'; (_) -> "" end, lists:seq(1, NumArgs))),
    [{uploaded_file, FileName, Location, Length}] = Req:post_files(),
    {ok, FileBytes} = file:read_file(Location),
    [Head|Rest] = admin_lib:parse_csv(FileBytes),
    RecordsToSave = lists:map(fun(Line) ->
                {_, Record} = lists:foldl(fun(Val, {Counter, Acc}) ->
                            AttrName = lists:nth(Counter, Head),
                            Attr = list_to_atom(AttrName),
                            {Counter + 1, Acc:Attr(Val)}
                    end, {1, DummyRecord}, Line),
                Record
        end, Rest),
    % TODO put these in a transaction
    lists:foldl(fun(Record, ok) ->
                {ok, _} = Record:save(),
                ok
        end, ok, RecordsToSave),
    {redirect, "/admin/model/"++ModelName}.

record('GET', [RecordId], Authorization) ->
    Record = boss_db:find(RecordId),
    AttributesWithDataTypes = lists:map(fun({Key, Val}) ->
                {Key, Val, boss_db:data_type(Key, Val)}
        end, Record:attributes()),
    {ok, [{'record', Record}, {'attributes', AttributesWithDataTypes}, 
            {'type', boss_db:type(RecordId)}, {timestamp, boss_mq:now("admin"++SessionID)}]}.

edit('GET', [RecordId], Authorization) ->
    Record = boss_db:find(RecordId),
    {ok, [{'record', Record}]};
edit('POST', [RecordId], Authorization) ->
    Record = boss_db:find(RecordId),
    NewRecord = lists:foldr(fun
            ('id', Acc) ->
                Acc;
            (Attr, Acc) ->
                AttrName = atom_to_list(Attr),
                Val = Req:post_param(AttrName),
                case lists:suffix("_time", AttrName) of
                    true ->
                        case Val of "now" -> Acc:Attr(erlang:now());
                            _ -> Acc
                        end;
                    false -> Acc:Attr(Val)
                end
        end, Record, Record:attribute_names()),
    case NewRecord:save() of
        {ok, SavedRecord} ->
            {redirect, "/admin/record/"++RecordId};
        {error, Errors} ->
            {ok, [{errors, Errors}, {record, NewRecord}]}
    end.

delete('GET', [RecordId], Authorization) ->
    {ok, [{'record', boss_db:find(RecordId)}]};
delete('POST', [RecordId], Authorization) ->
    Type = boss_db:type(RecordId),
    boss_db:delete(RecordId),
    {redirect, "/admin/model/" ++ atom_to_list(Type)}.

create(Method, [RecordType], Authorization) ->
    case lists:member(RecordType, boss_files:model_list()) of
        true ->
            Module = list_to_atom(RecordType),
            NumArgs = proplists:get_value('new', Module:module_info(exports)),
            DummyRecord = apply(list_to_atom(RecordType), 'new', lists:seq(1, NumArgs)),
            case Method of
                'GET' ->
                    {ok, [{type, RecordType}, {'record', DummyRecord}]};
                'POST' ->
                    Record = lists:foldr(fun
                                ('id', Acc) -> Acc:id('id');
                                (Attr, Acc) ->
                                    AttrName = atom_to_list(Attr),
                                    Val = Req:post_param(AttrName),
                                    Val1 = case lists:suffix("_time", AttrName) of
                                        true ->
                                            case Val of
                                                "now" -> erlang:now();
                                                _ -> ""
                                            end;
                                        _ -> Val
                                    end,
                                    Acc:Attr(Val1)
                            end, DummyRecord, DummyRecord:attribute_names()),
                    case Record:save() of
                        {ok, SavedRecord} ->
                            {redirect, "/admin/record/"++SavedRecord:id()};
                        {error, Errors} ->
                            {ok, [{errors, Errors}, {type, RecordType}, {'record', Record}]}
                    end
            end;
        _ ->
            {error, "Nonesuch model."}
    end.

lang('GET', [], Auth) ->
    Languages = boss_files:language_list(),
    {ok, [{lang_section, true}, {languages, Languages}]};
lang('GET', [Lang], Auth) ->
    OriginalLang = case application:get_env(assume_locale) of
        {ok, Val} -> Val;
        _ -> "en"
    end,
    Languages = boss_files:language_list(),
    {Untranslated, Translated} = boss_lang:extract_strings(Lang),
    LastModified = filelib:last_modified(boss_files:lang_path(Lang)),
    {ok, [{this_lang, Lang}, {languages, Languages},
            {original_lang, OriginalLang},
            {untranslated_messages, Untranslated},
            {translated_messages, Translated},
            {last_modified, LastModified},
		  	{lang_section, true}],
        [{"Cache-Control", "no-cache"}]};
lang('POST', [Lang|Fmt], Auth) ->
	WithBlanks = Req:post_param("trans_all_with_blanks"),
    LangFile = boss_files:lang_path(Lang),
    {ok, IODevice} = file:open(LangFile, [write, append]),
    lists:map(fun(Message) ->
                Original = proplists:get_value("orig", Message),
                Translation = proplists:get_value("trans", Message),
				BlockIdentifier = proplists:get_value("identifier", Message),
                case Translation of
                    "" -> 
						case WithBlanks of
							undefined -> ok;
							_ -> lang_write_to_file(IODevice, Original, Translation, BlockIdentifier)
						end;
                    _ -> lang_write_to_file(IODevice, Original, Translation, BlockIdentifier)
                end
        end, Req:deep_post_param(["messages"])),
    file:close(IODevice),
    boss_translator:reload(Lang),
    case Fmt of
        ["json"] -> {json, [{success, true}]};
        [] -> {redirect, "/admin/lang/"++Lang}
    end.

lang_write_to_file(IODevice, Original, Translation, BlockIdentifier) ->
	OriginalEncoded = unicode:characters_to_list(boss_lang:escape_quotes(Original)),
	TranslationEncoded = unicode:characters_to_list(boss_lang:escape_quotes(Translation)),
	case BlockIdentifier of
		undefined -> 
			file:write(IODevice, io_lib:format("\nmsgid \"~ts\"\n",[list_to_binary(OriginalEncoded)])),	   
			file:write(IODevice, io_lib:format("\msgstr \"~ts\"\n",[list_to_binary(TranslationEncoded)]));
		Identifier -> 
			file:write(IODevice, io_lib:format("\n#. ~ts\n",[list_to_binary(Identifier)])),
			file:write(IODevice, io_lib:format("msgid \"~s\"\n", [""])),
			{ok, OriginalTokens} = regexp:split(OriginalEncoded,"\r\n"),
			lang_write_multiline_to_file(IODevice, OriginalTokens),
			file:write(IODevice, io_lib:format("\msgstr \"~s\"\n", [""])),
			{ok, TranslationTokens} = regexp:split(TranslationEncoded,"\r\n"),
			lang_write_multiline_to_file(IODevice, TranslationTokens)
	end.

lang_write_multiline_to_file(IODevice, []) -> ok;
lang_write_multiline_to_file(IODevice, [Token|Rest]) ->
	ParsedToken = case Token of
		[] -> "";
		_ -> Token
	end,
	case Rest of
		[] -> file:write(IODevice, io_lib:format("\"~ts\"\n", [list_to_binary(ParsedToken)]));
		_ -> file:write(IODevice, io_lib:format("\"~ts~c~c\"\n", [list_to_binary(ParsedToken), 92, 110]))
	end,
	lang_write_multiline_to_file(IODevice, Rest).

create_lang('GET', [], Auth) ->
    {ok, [{lang_section, true}, {languages, boss_files:language_list()}]};
create_lang('POST', [], Auth) ->
    % TODO sanitize
    NewLang = Req:post_param("language"),
	boss_lang:create_lang(NewLang),
    {redirect, "/admin/lang/"++NewLang}.

delete_lang('GET', [Lang], Auth) ->
    {ok, [{lang_section, true}, {this_lang, Lang}]};
delete_lang('POST', [Lang], Auth) ->
	boss_lang:delete_lang(Lang),
    {redirect, "/admin/lang"}.

big_red_button('GET', [], Auth) ->
    Languages = lists:map(fun(Lang) ->
                {Untranslated, Translated} = boss_lang:extract_strings(Lang),
                [{code, Lang}, {untranslated_strings, Untranslated}]
        end, boss_files:language_list()),
    {ok, [{lang_section, true}, {languages, Languages}, {strings, boss_lang:extract_strings()}]}.

upgrade('GET', [], Auth) ->
    {ok, [ {upgrade_section, true} ]};
upgrade('POST', [], Auth) ->
    Modules = [M || {M, F} <- code:all_loaded(), is_list(F), not code:is_sticky(M)],
    error_logger:info_msg("Reloading ~p modules...~n", [erlang:length(Modules)]),
    [begin code:purge(M), code:load_file(M) end || M <- Modules],
    error_logger:info_msg("Reloading routes...~n"),
	boss_router:reload(),
	error_logger:info_msg("Reloading translation...~n"),
	boss_translator:reload_all(),
    {redirect, "/admin/upgrade"}.

reread_news_script('POST', [], Auth) ->
	ok = boss_record_compiler:compile(filename:join(["init", "news.erl"]), []),
    boss_news:reset(),
    {redirect, "/admin/upgrade"}.


news_api('POST', ["created", Id], Auth) ->
    ok = boss_news:created(Id, Req:post_params("new")),
    {output, "ok"};
news_api('POST', ["updated", Id], Auth) ->
    ok = boss_news:updated(Id, Req:post_params("old"), Req:post_params("new")),
    {output, "ok"};
news_api('POST', ["deleted", Id], Auth) ->
    ok = boss_news:deleted(Id, Req:post_params("old")),
    {output, "ok"}.
