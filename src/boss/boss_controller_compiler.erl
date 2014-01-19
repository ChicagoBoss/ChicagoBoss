-module(boss_controller_compiler).
-export([compile/1, compile/2, add_routes_to_forms/1]).
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-compile(export_all).
-endif.
-compile(export_all).

-type error(T)           :: {ok, T} | {error, string()}.
-type syntaxTree()       :: erl_syntax:syntaxTree().
-type name()             :: atom()|[byte(),...].
-type fctn_n()           :: {atom(), non_neg_integer()}.
-type fctn()             :: {function, atom(), atom(), non_neg_integer(), _}.
-type pair()             :: {atom(),atom()}.

-type token_ast_var()    :: {var, name(), string()}.
-type token_ast_match()  :: {match, _, _, token_ast_var()}.
-type token_ast_string() :: {string,_, [char(),...]}.
-type token_ast_cons()   :: nil|{cons,_, 
                                 token_ast_var() | token_ast_match() | token_ast_string(),
                                 token_ast_cons()}.


-spec compile(binary() | [atom() | [any()] | char()]) -> any().
-spec compile(binary() | [atom() | [any()] | char()],[any()]) -> any().
-spec add_routes_to_forms([any()]) -> [any(),...].
-spec add_routes_to_forms([any()],[any()],[{_,[any()]}]) -> [any(),...].
-spec add_export_to_forms([any(),...]) -> [any(),...].
-spec add_export_to_forms([any(),...],[any()]) -> [any(),...].
-type name_clause() :: {clause, _, [list(string()),...], _,_}.

-spec extract_routes_from_clauses(string(),[token_ast_cons(),...]) -> [string()].
-spec extract_routes_from_clauses(string(),[any()],[{_,[any()]}])  -> [string()].

-spec route_from_token_ast(token_ast_cons())                       -> [string()].
-spec route_from_token_ast(_,[any()])                              -> [any()].
-spec function_for_routes([{name(),[name()]}])                     -> [syntaxTree()].
-spec map_syntax_tuples([{name(),[name()]}])                       -> syntaxTree().
-spec map_tokens([name()])                                         -> syntaxTree().

compile(File) ->
    compile(File, []).

compile(File, Options) ->
    lager:info("Compile controller ~s with options ~p", [File,Options]), 
    boss_compiler:compile(File,
                          [debug_info,{pre_revert_transform, fun ?MODULE:add_routes_to_forms/1}|Options]).

add_routes_to_forms(Forms) ->
    [{eof, _Line}|OtherForms]	= lists:reverse(Forms),
    Forms1			= add_export_to_forms(lists:reverse(OtherForms)),
    add_routes_to_forms(Forms1, [], []).

add_routes_to_forms([], FormAcc, RouteAcc) ->
    RoutesFunction = function_for_routes(lists:reverse(RouteAcc)),
    lists:reverse(FormAcc, RoutesFunction);
add_routes_to_forms([{function, _, Name, Arity, Clauses} = Fxn|Rest], FormAcc, RouteAcc) 
  when Arity =:= 2; Arity =:= 3 ->
    NewRoutes = extract_routes_from_clauses(Name, Clauses),
    add_routes_to_forms(Rest, [Fxn|FormAcc], lists:reverse(NewRoutes, RouteAcc));
add_routes_to_forms([H|T], FormAcc, RouteAcc) ->
    add_routes_to_forms(T, [H|FormAcc], RouteAcc).

add_export_to_forms(Forms) ->
    add_export_to_forms(Forms, []).

add_export_to_forms([{attribute, _, module, _} = H | T], LeadingForms) ->
    lists:reverse(LeadingForms, [H, {attribute, {0, 2}, export, [{'_routes', 0}]} | T]);
add_export_to_forms([H|T], LeadingForms) ->
    add_export_to_forms(T, [H|LeadingForms]).

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
			 [erl_syntax:clause([], 
					    none,
					    [erl_syntax:list(map_syntax_tuples(Routes))])])].

map_syntax_tuples(Routes) ->
    lists:map(fun({Name, Tokens}) ->
		      erl_syntax:tuple([
					erl_syntax:atom(Name),
					erl_syntax:list(map_tokens(Tokens))
				       ])
              end, Routes).


map_tokens(Tokens) ->
    lists:map(fun (T) when is_atom(T) ->
		      erl_syntax:atom(T);
		  (T) when is_list(T) ->
		      erl_syntax:string(T)
              end, Tokens).
