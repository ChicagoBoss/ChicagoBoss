-module(admin_controller, [Req]).
-export(['_auth'/1, model/3, record/3, delete/3, create/3]).
-define(RECORDS_PER_PAGE, 100).

'_auth'(_) ->
    case Req:peer_ip() of
        {192, 168, _, _} ->
            {ok, local};
        {127, 0, 0, 1} ->
            {ok, local};
        _ ->
            {redirect, "/admin/access_denied"}
    end.

model('GET', [], Authorization) ->
    {ok, [{records, []}, {models, model_list()}, {this_model, ""}]};
model('GET', [ModelName], Authorization) ->
    model('GET', [ModelName, "1"], Authorization);
model('GET', [ModelName, PageName], _) ->
    Page = list_to_integer(PageName),
    Model = list_to_atom(ModelName),
    RecordCount = boss_db:count(Model),
    Records = boss_db:find(Model, [], ?RECORDS_PER_PAGE, (Page - 1) * ?RECORDS_PER_PAGE, primary, str_descending),
    Pages = lists:seq(1, ((RecordCount-1) div ?RECORDS_PER_PAGE)+1),
    {ok, 
        [{records, Records}, {models, model_list()}, {this_model, ModelName}, 
            {pages, Pages}, {this_page, Page}], 
        [{"Cache-Control", "no-cache"}]}.

record('GET', [RecordId], Authorization) ->
    {ok, [{'record', boss_db:find(RecordId)}, {'type', boss_db:type(RecordId)}]}.

delete('GET', [RecordId], Authorization) ->
    {ok, [{'record', boss_db:find(RecordId)}]};
delete('POST', [RecordId], Authorization) ->
    Type = boss_db:type(RecordId),
    boss_db:delete(RecordId),
    {redirect, "/admin/model/" ++ atom_to_list(Type)}.

create(Method, [RecordType], Authorization) ->
    case lists:member(RecordType, model_list()) of
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
                    {redirect, "/admin/record/"++(Record:save()):id()}
            end;
        _ ->
            {error, "Nonesuch model."}
    end.

% internal

model_list() ->
    ModelPath = filename:join([filename:dirname(code:which(?MODULE)), "..", "Model"]),
    {ok, Files} = file:list_dir(ModelPath),
    lists:sort(lists:map(fun(X) -> filename:basename(X, ".erl") end, 
        lists:filter(fun
            ("."++_) -> 
                false; 
            (File) -> lists:suffix(".erl", File) 
        end, Files))).
