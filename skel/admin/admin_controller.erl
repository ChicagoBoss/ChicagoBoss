-module(admin_controller, [Req]).
-compile(export_all).
-define(RECORDS_PER_PAGE, 100).

before_(_) ->
	ClientIp = case Req:header(x_forwarded_for) of
				   undefined -> Req:peer_ip();
				   IP -> IP
			   end,
    case ClientIp of
        {192, 168, _, _} ->
            {ok, local};
        {127, 0, 0, 1} ->
            {ok, local};
        {10, 0, _, _} ->
            {ok, local};
        _ ->
            {redirect, "/admin/access_denied"}
    end.

index('GET', [], Authorization) ->
	[{loaded, ModulesLoaded}, _, _, _, _, _] = application:info(),
	ConfigValues = [ [{Key, Value}] || {Key, Value} <- application:get_all_env()],
	SystemValues = [ {otp_release, erlang:system_info(system_version)},
					 {processors, erlang:system_info(logical_processors_online)} ],
    {ok, [ {index_section, true}, {modules_loaded, ModulesLoaded}, {config_env, ConfigValues}, {system_env, SystemValues}] }.

routes('GET', [], Authorization) ->
    {ok, [ {routes_section, true}, {routes, boss_router:get_all()} ]};
routes('GET', ["reload"], Authorization) ->
    boss_router:reload(),
    boss_flash:add(Req, notice, "Routes reloaded"),
    {redirect, boss_router:base_url() ++ "/admin/routes"}.

heartbeat('POST', [WatchName], Authorization) ->
    boss_news:extend_watch(list_to_integer(WatchName)),
    {output, ""}.

watch('POST', [], Authorization) ->
    SessionID = Req:session_id(),
    TopicString = Req:post_param("topic_string"),
    {ok, WatchId} = boss_news:watch(TopicString, fun(updated, {Record, Attr, OldVal, NewVal}) ->
                boss_mq:push("admin" ++ SessionID, [{ev, updated}, {data, [{id, Record:id()}, {attr, Attr}, {val, NewVal}]}])
        end, 60),
    {json, [{watch_id, WatchId}]}.

events('GET', [Since], Authorization) ->
    io:format("Pulling events...~n", []),
    SessionID = Req:session_id(),
    {ok, Timestamp, Messages} = boss_mq:pull("admin" ++ SessionID, now_from_micro_seconds(list_to_integer(Since)), 30),
    {json, [{messages, Messages}, {timestamp, micro_seconds(Timestamp)}]}.

model('GET', [], Authorization) ->
    {ok, [{model_section, true}, {records, []}, {models, boss_files:model_list()}, {this_model, ""}]};
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
            {topic_string, TopicString}, {timestamp, micro_seconds(erlang:now())}], 
        [{"Cache-Control", "no-cache"}]}.

record('GET', [RecordId], Authorization) ->
    Record = boss_db:find(RecordId),
    AttributesWithDataTypes = lists:map(fun({Key, Val}) ->
                {Key, Val, boss_db:data_type(Key, Val)}
        end, Record:attributes()),
    {ok, [{'record', Record}, {'attributes', AttributesWithDataTypes}, 
            {'type', boss_db:type(RecordId)}, {timestamp, micro_seconds(erlang:now())}]}.

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
            case Method of
                'GET' ->
                    Record = apply(list_to_atom(RecordType), 'new', lists:seq(1, NumArgs)),
                    {ok, [{type, RecordType}, {'record', Record}]};
                'POST' ->
                    DummyRecord = apply(list_to_atom(RecordType), 'new', lists:seq(1, NumArgs)),
                    Record = apply(list_to_atom(RecordType), 'new', 
                        lists:map(fun('id') -> 'id'; 
                                (A) ->
                                    AttrName = atom_to_list(A),
                                    Val = Req:post_param(AttrName),
                                    case lists:suffix("_time", AttrName) of
                                        true ->
                                            case Req:post_param(AttrName) of
                                                "now" -> erlang:now();
                                                _ -> ""
                                            end;
                                        _ -> Val
                                    end
                            end, DummyRecord:attribute_names())),
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
	Mode =  case Req:post_param("trans_all_with_blanks") of
				undefined -> filled;
				_ -> all
			end,
	boss_lang:update_po(Lang, Mode, Req:deep_post_param(["messages"])),
    boss_translator:reload(Lang),
    case Fmt of
        ["json"] -> {json, [{success, true}]};
        [] -> {redirect, "/admin/lang/"++Lang}
    end.

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


micro_seconds({Mega, Secs, Micro}) ->
    Mega * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + Micro.

now_from_micro_seconds(MicroSeconds) ->
    {MicroSeconds div (1000 * 1000 * 1000 * 1000), MicroSeconds div (1000 * 1000), MicroSeconds}.
