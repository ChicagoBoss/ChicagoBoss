-module(boss_json).
-export([encode/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec encode(_,_) -> any().
-spec json_data1([{_,_}],_,[{_,_}]) -> {'struct',[{_,_}]}.

encode([First|_] = Data, ModelList) ->
    case boss_model_manager:is_model_instance(First, ModelList) of
        true ->
            mochijson2:encode(lists:map(fun boss_model_manager:to_json/1, Data));
        false ->
            mochijson2:encode(json_data1(Data, ModelList, []))
    end;

encode([],_) ->
    "";

encode(Data, ModelList) ->
    case boss_model_manager:is_model_instance (Data, ModelList) of
        true ->
            mochijson2:encode(boss_model_manager:to_json(Data));
        false ->
            mochijson2:encode(json_data1(Data, ModelList, []))
    end.


json_data1([], _, Acc) ->
    {struct, lists:reverse(Acc)};

json_data1([{VariableName, [First|_] = Variable}|Rest], ModelList, Acc) when is_integer(First) ->
    json_data1(Rest, ModelList, [{VariableName, list_to_binary(Variable)}|Acc]);

json_data1([{VariableName, [First|_] = Variable}|Rest], ModelList, Acc) when is_tuple(First) ->
    case boss_model_manager:is_model_instance (First, ModelList) of
        true ->
            json_data1(Rest, ModelList, [{VariableName, lists:map(fun boss_model_manager:to_json/1, Variable)}|Acc]);
        false ->
            json_data1(Rest, ModelList, [{VariableName, json_data1(Variable, ModelList, [])}|Acc])
    end;

json_data1([{VariableName, [[{_, _}|_]|_] = Variable}|Rest], ModelList, Acc) ->
    json_data1(Rest, ModelList, [{VariableName, lists:map(fun(Item) -> 
                            json_data1(Item, ModelList, [])
                    end, Variable)}|Acc]);

%% It's an erlang:now() value.
json_data1([{VariableName, {A, B, C} = Val}|Rest], ModelList, Acc) when is_integer(A), is_integer(B), is_integer(C) ->
    json_data1(Rest, ModelList, [{VariableName, iso8601:format(Val)}|Acc]);
json_data1([{VariableName, Variable}|Rest], ModelList, Acc) ->
    case boss_model_manager:is_model_instance (Variable, ModelList) of
        true -> 
            json_data1(Rest, ModelList, [{VariableName, boss_model_manager:to_json(Variable)}|Acc]);
        false -> 
            json_data1(Rest, ModelList, [{VariableName, Variable}|Acc])
    end.
