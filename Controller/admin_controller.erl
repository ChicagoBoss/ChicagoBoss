-module(admin_controller, [Req]).
-export([third_arg/1, model/3, record/3, delete/3, create/3]).

third_arg(_) ->
    case Req:get(peer) of
        "192.168."++_ ->
            {ok, local};
        "127.0.0.1" ->
            {ok, local};
        "64.81.136.220" ->
            {ok, remote};
        _ ->
            {redirect, "/admin/access_denied"}
    end.

model('GET', [], Authorization) ->
    {ok, [{records, []}, {models, model_list()}, {this_model, ""}]};
model('GET', [ModelName], Authorization) ->
    model('GET', [ModelName, 0], Authorization);
model('GET', [ModelName, Offset], _) ->
    Records = boss_db:find(list_to_atom(ModelName), [], 100, Offset, primary, str_descending),
    {ok, [{records, Records}, {models, model_list()}, {this_model, ModelName}]}.

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
                                    Val = Req:get_post_value(AttrName),
                                    case lists:suffix("_time", AttrName) of
                                        true ->
                                            case Req:get_post_value(AttrName) of
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
