-module(boss_controller_compiler).

-compile(export_all).

compile(File) ->
    compile(File, []).

compile(File, Options) ->
    boss_compiler:compile(File,
        [{pre_revert_transform, fun ?MODULE:add_routes_to_forms/1}|Options]).

add_routes_to_forms(Forms) ->
    [{eof, _Line}|OtherForms] = lists:reverse(Forms),
    add_routes_to_forms(lists:reverse(OtherForms), [], []).

add_routes_to_forms([], FormAcc, RouteAcc) ->
    RoutesFunction = function_for_routes(lists:reverse(RouteAcc)),
    lists:reverse(FormAcc, RoutesFunction);
add_routes_to_forms([{function, _, Name, Arity, Clauses} = Fxn|Rest], FormAcc, RouteAcc) when Arity =:= 2; Arity =:= 3 ->
    NewRoutes = extract_routes_from_clauses(Name, Clauses),
    add_routes_to_forms(Rest, [Fxn|FormAcc], lists:reverse(NewRoutes, RouteAcc));
add_routes_to_forms([H|T], FormAcc, RouteAcc) ->
    add_routes_to_forms(T, [H|FormAcc], RouteAcc).

extract_routes_from_clauses(Name, Clauses) ->
    extract_routes_from_clauses(Name, Clauses, []).

extract_routes_from_clauses(_, [], Acc) ->
    lists:reverse(Acc);
extract_routes_from_clauses(Name, [{clause, _, 
            [_, URLTokens|_], _, _}|Rest], Acc) ->
    Route = route_from_token_ast(URLTokens),
    extract_routes_from_clauses(Name, Rest, [{Name, Route}|Acc]);
extract_routes_from_clauses(Name, [_H|T], Acc) ->
    extract_routes_from_clauses(Name, T, Acc).

route_from_token_ast(Tokens) ->
    route_from_token_ast(Tokens, []).

route_from_token_ast({cons, _, {var, _, VariableName}, T}, Acc) ->
    route_from_token_ast(T, [VariableName|Acc]);
route_from_token_ast({cons, _, {match, _, _, {var, _, VariableName}}, T}, Acc) ->
    route_from_token_ast(T, [VariableName|Acc]);
route_from_token_ast({cons, _, {string, _, String}, T}, Acc) ->
    route_from_token_ast(T, [String|Acc]);
route_from_token_ast(_, Acc) ->
    lists:reverse(Acc).

function_for_routes(Routes) ->
    [erl_syntax:function(erl_syntax:atom('_routes'),
            [erl_syntax:clause([], none,
                    [erl_syntax:list(lists:map(fun({Name, Tokens}) ->
                                        erl_syntax:tuple([
                                                erl_syntax:atom(Name),
                                                erl_syntax:list(lists:map(fun
                                                            (T) when is_atom(T) ->
                                                                erl_syntax:atom(T);
                                                            (T) when is_list(T) ->
                                                                erl_syntax:string(T)
                                                        end, Tokens))
                                            ])
                                end, Routes))])])].
