-module(erlydtl_runtime).

-compile(export_all).

find_value(Key, L) when is_list(L) ->
    case proplists:get_value(Key, L) of
        undefined ->
            case proplists:get_value(atom_to_list(Key), L) of
                undefined ->
                    proplists:get_value(list_to_binary(atom_to_list(Key)), L);
                Val -> Val
            end;
        Val -> Val
    end;
find_value(Key, {GBSize, GBData}) when is_integer(GBSize) ->
    case gb_trees:lookup(Key, {GBSize, GBData}) of
        {value, Val} ->
            Val;
        _ ->
            undefined
    end;
find_value(Key, Tuple) when is_tuple(Tuple) ->
    Module = element(1, Tuple),
    case Module of
        dict -> 
            case dict:find(Key, Tuple) of
                {ok, Val} ->
                    Val;
                _ ->
                    undefined
            end;
        Module ->
            case proplists:get_value(Key, Module:module_info(exports)) of
                1 ->
                    Tuple:Key();
                _ ->
                    undefined
            end
    end.

fetch_value(Key, Data) ->
    case find_value(Key, Data) of
        undefined ->
            throw({undefined_variable, Key});
        Val ->
            Val
    end.

are_equal(Arg1, Arg2) when Arg1 =:= Arg2 ->
    true;
are_equal(Arg1, Arg2) when is_binary(Arg1) ->
    are_equal(binary_to_list(Arg1), Arg2);
are_equal(Arg1, Arg2) when is_binary(Arg2) ->
    are_equal(Arg1, binary_to_list(Arg2));
are_equal(Arg1, Arg2) when is_integer(Arg1) ->
    are_equal(integer_to_list(Arg1), Arg2);
are_equal(Arg1, Arg2) when is_integer(Arg2) ->
    are_equal(Arg1, integer_to_list(Arg2));
are_equal([Arg1], Arg2) when is_list(Arg1) ->
    are_equal(Arg1, Arg2);
are_equal(Arg1, [Arg2]) when is_list(Arg1) ->
    are_equal(Arg1, Arg2);
are_equal(Arg1, Arg2) when is_atom(Arg1), is_list(Arg2) ->
	 are_equal(atom_to_list(Arg1), Arg2);
are_equal(Arg1, Arg2) when is_list(Arg1), is_atom(Arg2) ->
	 are_equal(Arg1, atom_to_list(Arg2));
are_equal(_, _) ->
    false.

is_false("") ->
    true;
is_false(false) ->
    true;
is_false(undefined) ->
    true;
is_false("0") ->
    true;
is_false(<<"0">>) ->
    true;
is_false(<<>>) ->
    true;
is_false(_) ->
    false.

stringify_final(In) ->
   stringify_final(In, []).
stringify_final([], Out) ->
   lists:reverse(Out);
stringify_final([El | Rest], Out) when is_atom(El) ->
   stringify_final(Rest, [atom_to_list(El) | Out]);
stringify_final([El | Rest], Out) ->
   stringify_final(Rest, [El | Out]).

init_counter_stats(List) ->
    init_counter_stats(List, undefined).

init_counter_stats(List, Parent) ->
    [{counter, 1}, 
        {counter0, 0}, 
        {revcounter, length(List)}, 
        {revcounter0, length(List) - 1}, 
        {first, true}, 
        {last, length(List) =:= 1},
        {parentloop, Parent}].

increment_counter_stats([{counter, Counter}, {counter0, Counter0}, {revcounter, RevCounter},
        {revcounter0, RevCounter0}, {first, _}, {last, _}, {parentloop, Parent}]) ->
    [{counter, Counter + 1},
        {counter0, Counter0 + 1},
        {revcounter, RevCounter - 1},
        {revcounter0, RevCounter0 - 1},
        {first, false}, {last, RevCounter0 =:= 1},
        {parentloop, Parent}].

cycle(NamesTuple, Counters) when is_tuple(NamesTuple) ->
    element(fetch_value(counter0, Counters) rem size(NamesTuple) + 1, NamesTuple).
