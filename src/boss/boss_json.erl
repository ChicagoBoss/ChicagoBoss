-module(boss_json).
-export([encode/2]).

encode([First|_] = Data, ModelList) ->
    case boss_record_list:is_boss_record(First, ModelList) of
        true ->
            mochijson2:encode(lists:map(fun boss_record_to_json/1, Data));
        false ->
            mochijson2:encode(json_data1(Data, ModelList, []))
    end.

json_data1([], _, Acc) ->
    {struct, lists:reverse(Acc)};
json_data1([{VariableName, [First|_] = Variable}|Rest], ModelList, Acc) when is_integer(First) ->
    json_data1(Rest, ModelList, [{VariableName, list_to_binary(Variable)}|Acc]);
json_data1([{VariableName, [First|_] = Variable}|Rest], ModelList, Acc) when is_tuple(First) ->
    case boss_record_lib:is_boss_record(First, ModelList) of
        true ->
            json_data1(Rest, ModelList, [{VariableName, lists:map(fun boss_record_to_json/1, Variable)}|Acc]);
        false ->
            json_data1(Rest, ModelList, [{VariableName, json_data1(Variable, ModelList, [])}|Acc])
    end;
json_data1([{VariableName, [First|_] = Variable}|Rest], ModelList, Acc) when is_list(First) ->
    json_data1(Rest, ModelList, [{VariableName, lists:map(fun(Item) -> 
                            json_data1(Item, ModelList, [])
                    end, Variable)}|Acc]);
json_data1([{VariableName, {A, B, C} = Val}|Rest], ModelList, Acc) when is_integer(A), is_integer(B), is_integer(C) ->
    json_data1(Rest, ModelList, [{VariableName, list_to_binary(erlydtl_filters:date(calendar:now_to_datetime(Val), "F d, Y H:i:s"))}|Acc]);
json_data1([{VariableName, Variable}|Rest], ModelList, Acc) ->
    case boss_record_lib:is_boss_record(Variable, ModelList) of
        true -> 
            json_data1(Rest, ModelList, [{VariableName, boss_record_to_json(Variable)}|Acc]);
        false -> 
            json_data1(Rest, ModelList, [{VariableName, Variable}|Acc])
    end.

boss_record_to_json(Variable) ->
    Data = lists:map(fun
            ({Attr, Val}) when is_list(Val) ->
                {Attr, list_to_binary(Val)};
            ({Attr, {{_, _, _}, {_, _, _}} = Val}) ->
                {Attr, list_to_binary(erlydtl_filters:date(Val, "F d, Y H:i:s"))};
            (Other) ->
                Other
        end, Variable:attributes()),
    {struct, Data}.
