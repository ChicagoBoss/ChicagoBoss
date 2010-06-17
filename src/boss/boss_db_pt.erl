% Parse transform for calls to boss_db:find.
% Derived from lyet.erl, which was derived from ct_expand.erl

-module(boss_db_pt).
-compile(export_all).

-export([function/4,
         format_error/1]).
-export([parse_transform/2]).


-define(ERROR(R, T, F, I),
        begin
            rpt_error(R, T, F, I),
            throw({error,erl_syntax:get_pos(
                           proplists:get_value(form,I)),{unknown,R}})
        end).


-import(erl_syntax, [clause/3,
                     clause_patterns/1,
                     clause_body/1,
                     clause_guard/1,
                     match_expr/2,
                     function_clauses/1,
                     get_pos/1,
                     add_ann/2,
                     get_ann/1]).

parse_transform(Forms, Options) ->
    TransformOperatorsFun = fun
        (Form, _Context) ->
            RevertedForm = erl_syntax:revert(Form),
            {call, Location, Invocation, Body} = RevertedForm,
            case Body of
                [Type, Conditions|Rest] ->
                    NewConditions = replace_operators(Conditions),
                    {call, Location, Invocation, [Type, NewConditions|Rest]};
                _ ->
                    RevertedForm
            end
    end,
    function({boss_db, find, any}, TransformOperatorsFun, 
        function({boss_db, count, 2}, TransformOperatorsFun, Forms, Options), 
        Options).

replace_operators({cons, _Location, {match, MatchLoc, LHS, RHS}, Rest}) ->
    LHSLoc = element(2, LHS),
    RHSLoc = element(2, RHS),
    {cons, LHSLoc, LHS,
        {cons, MatchLoc, {atom, MatchLoc, 'equal'},
            {cons, RHSLoc, RHS, replace_operators(Rest)}}};
replace_operators({cons, _Location, {op, OpLoc, Operator, LHS, RHS}, Rest}) when Operator =:= '>'; Operator =:= '<' ->
    LHSLoc = element(2, LHS),
    RHSLoc = element(2, RHS),
    {cons, LHSLoc, LHS,
        {cons, OpLoc, {atom, OpLoc, Operator},
            {cons, RHSLoc, RHS, replace_operators(Rest)}}};
replace_operators({cons, Location, First, Rest}) ->
    {cons, Location, First, replace_operators(Rest)};
replace_operators(Other) ->
    Other.

%%% API: function({Module, Function, Arity}, Fun, Forms, Options) ->
%%%         NewForms
%%%
%%% Forms and Options are the arguments passed to the parse_transform/2
%%% function.
%%% {Module, Function, Arity} is the function call to transform
%%% Fun(Form, Context) -> NewForm is the fun provided by the caller.
%%%
%%% Context is a property list, containing the following properties:
%%% - {file, Filename}
%%% - {module, ModuleName}
%%% - {function, FunctionName}       % name of the enclosing function
%%% - {arity, Arity :: integer()}    % arity of same
%%% - {var_names, Vars :: [atom()]}  % generated variables binding the
%%%                                  % function arguments.
%%%                                  % length(Vars) == Arity
%%%
function({_Module, _Function, _Arity} = MFA, F,
         Forms, Options) when is_function(F) ->
    parse_transform(MFA, F, Forms, Options).

parse_transform(MFA, Fun, Forms, _Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try begin
            NewTree = xform(MFA, Fun, Forms, [{file, File}]),
            revert_tree(NewTree)
        end
    catch
        throw:{error,Ln,What} ->
            {error, [{File, [{Ln,?MODULE,What}]}], []}
    end.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].

format_error(Other) ->
    lists:flatten(
      io_lib:format("unknown error in parse_transform: ~p", [Other])).

xform({M,F,A}, Fun, Forms, Context0) ->
    Bef = fun(function, Form, Ctxt) ->
                  {Fname, Arity} = erl_syntax_lib:analyze_function(Form),
                  VarNames = erl_syntax_lib:new_variable_names(
                               Arity,
                               fun (N) ->
                                 list_to_atom ("_V" ++ integer_to_list (N))
                               end,
                               erl_syntax_lib:variables(Form)),
                  {Form, [{function, Fname},
                          {arity, Arity},
                          {var_names, VarNames}|Ctxt]};
             (_, Form, Context) ->
                  {Form, Context}
          end,
    Aft = fun(application, Form, Context) ->
                  case erl_syntax_lib:analyze_application(Form) of
                      {M, {F, _}} when A =:= any ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      {M, {F, A}} ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      {F, A} when M =:= "local" ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      {F, _} when M =:= "local", A =:= any ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      _ ->
                          Form
                  end;
             (function, Form, Context) ->
                  Form1 =
                      erl_syntax_lib:map_subtrees(
                        fun(Clause) ->
                                case should_i_bind(Clause) of
                                    true ->
                                        Pats = clause_patterns(Clause),
                                        CBod = clause_body(Clause),
                                        CGd = clause_guard(Clause),
                                        Pats1 =
                                            lists:zipwith(
                                              fun(V, P) ->
                                                      match_expr(v(V), P)
                                              end,
                                              proplists:get_value(
                                                var_names, Context),
                                              Pats),
                                        clause(Pats1, CGd, CBod);
                                    false ->
                                        Clause
                                end
                        end, Form),
                  Form1;
             (_, Form, _Context) ->
                  Form
          end,
    [Module] = [Mx || {attribute, _, module, Mx} <- Forms],
    transform(Forms, Bef, Aft, [{module, Module}|Context0]).

transform(Forms, Before, After, Context) ->
    F1 =
        fun(Form) ->
                Type = erl_syntax:type(Form),
                {Form1, Context1} =
                    try Before(Type, Form, Context)
                    catch
                        error:Reason ->
                            ?ERROR(Reason, 'before', Before, 
                                   [{type, Type},
                                    {context, Context},
                                    {form, Form}])
                    end,
                Form2 =
                    case erl_syntax:subtrees(Form1) of
                        [] ->
                            Form1;
                        List ->
                            NewList =
                                transform(
                                  List, Before, After, Context1),
                            erl_syntax:update_tree(Form, NewList)
                    end,
                Type2 = erl_syntax:type(Form2),
                try After(Type2, Form2, Context1)
                catch
                    error:Reason2 ->
                        ?ERROR(Reason2, 'after', After, 
                               [{type, Type2},
                                {context, Context1},
                                {form, Form2}])
                end
        end,
    F2 = fun(List) when is_list(List) ->
                 map(F1, List);
            (Form) ->
                 F1(Form)
         end,
    map(F2, Forms).

%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
map(F, [Hd|Tail]) ->
    {Before, Res, After} =
        case F(Hd) of
            {Be, _, Af} = Result when is_list(Be), is_list(Af) ->
                Result;
            R1 ->
                {[], R1, []}
        end,
    Rs = map(F, Tail),
    Before ++ [Res| After ++ Rs];
map(F, []) when is_function(F, 1) -> [].

rpt_error(Reason, BeforeOrAfter, Fun, Info) ->
    Fmt = lists:flatten(
            ["*** ERROR in parse_transform function:~n"
             "*** Reason     = ~p~n"
             "*** applying ~w fun (~p)~n",
             ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, BeforeOrAfter, Fun | 
            lists:foldr(
              fun({K,V}, Acc) ->
                      [K, V | Acc]
              end, [], Info)],
    io:format(Fmt, Args).

should_i_bind(Tree) ->
    erl_syntax_lib:fold(
      fun(T, Flag) ->
              lists:member(bind_state, get_ann(T)) or Flag
      end, false, Tree).

v(V) ->
    erl_syntax:variable(V).
