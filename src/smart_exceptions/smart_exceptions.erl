%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @copyright Copyright(C) 2003-2008 Thomas Lindgren <thomasl_erlang@yahoo.com>.
%% @license
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%%
%%			   SMART EXCEPTIONS
%%
%% Author: Thomas Lindgren (030414-; 051016; 081004 [2.0])
%%
%% A simplified version of the earlier smart_exceptions, twice.
%%
%% USAGE
%%   erlc +'{parse_transform, smart_exceptions}' file.erl
%%
%% As given, the code generates a "smart exit", an exit with more info
%% than is usual. Uncomment the -define(exn_handler,...) to instead
%% invoke ?default_exn_handler_mod:* when there is an exit.
%%
%% PURPOSE:
%%
%% Rather than generating a terse exception 'badarg', this preprocessor
%% rewrites exceptions (apart from 'function undefined') to do one of:
%%  - invoke an exception handler, smart_exc_rt.erl (or user defined)
%%    * includes giving some BIF context
%%  - generate a 'big exit' with module, function, line, reason
%%
%% The generated code looks awful (lots of redundant code) but the beam
%% compiler gets rid of this.
%%
%% Third version of smart_exceptions: here, we get rid of the Handler
%% argument and the R9/R10 stuff (we always use try ... end) as well
%% as the use of the mapform module (which is proprietary). This means
%% more straightforward code and easy use in R12, as well as getting
%% rid of a pesky bug regarding throw/1.
%%   This is set as version {2,0,0}.
%%
%% UNFINISHED
%% - we assume that BIF, operator, binary expr and clause match
%%   failures generate (only) errors, not exits; this seems to be
%%   the case but has not been verified
%% - we assume default types and widths for bin exprs, can we do that?
%% - list comprehensions?

-module(smart_exceptions).
-author('thomasl_erlang@yahoo.com').
-export([parse_transform/2]).
-version({2,0,0}).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Current typical usage:
%%   erlc -pa $PATH +'{parse_transform, smart_exceptions}' $ERLFILE
%%
%% PATH must provide smart_exceptions.beam to enable the parse transform.
%%
%% If you want to see what the transform produces, provide the flag '-E'.

parse_transform(Forms, Opts) ->
    %% Opts = compiler options
    M = get_module_name(Forms),
    forms(M, Forms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file(File, Opts) ->
    {Mod, Exp, Forms, Misc} = parse:file(File, Opts),
    NewForms = forms(Mod, Forms),
    NewMod = {Mod, Exp, NewForms, Misc},
    parse:print(NewMod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forms(M, Forms0) ->
    Forms = simple_resolve_imports(Forms0),
    [ form(M, Form) || Form <- Forms ].

form(M, {function, Line, F, A, _Clss} = Form) ->
    mapform0(
      fun({function, Lf, F1, A1, []}=Form0) ->
	      Form0;
	 ({function, Lf, F1, A1, Clss}) ->
	      smart_function(M, F1, A1, Lf, Clss);
	 ({match, Lm, P, E}=Expr) ->
	      smart_match(M, F, A, Lm, P, E);
	 ({'case',Lc,E,Clss}) ->
	      smart_case(M, F, A, Lc, E, Clss);
	 ({'if',Li,Clss}) ->
	      smart_if(M, F, A, Li, Clss);
	 ({'fun',Lf,{clauses,Clss}}) ->
	      smart_fun(M, F, A, Lf, Clss);
	 ({'fun',Lf,{clauses,Clss}, Info}) ->
	      smart_fun(M, F, A, Lf, Clss, Info);
	 ({op,Lo,Op,E1,E2}=E) ->
	      smart_binop(M, F, A, Lo, Op, E1, E2);
	 ({op,Lo,Op,E1}=E) ->
	      smart_unop(M, F, A, Lo, Op, E1);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,exit}},[Rsn]}=E) ->
	      smart_exit(M, F, A, Lc, Rsn);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,fault}},[Rsn]}=E) ->
	      smart_fault(M, F, A, Lc, Rsn);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,error}},[Rsn]}=E) ->
	      smart_error(M, F, A, Lc, Rsn);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,throw}},[Rsn]}=E) ->
	      E;
	 ({call,Lc,{atom,Lf,exit},[Rsn]}=E) ->
	      smart_exit(M, F, A, Lc, Rsn);
	 ({call,Lc,{atom,Lf,throw},[Rsn]}=E) ->
	      E;
	 ({call,Lc,{remote,Lr,{atom,Lm,Mod},{atom,Lf,Fn}},As}=E) ->
	      case erlang:is_builtin(Mod, Fn, length(As)) of
		  true ->
		      smart_bif(M, F, A, Lc, 
				Mod, Fn, length(As), As);
		  false ->
		      E
	      end;
	 ({bin, Lb, BinElts}=E) ->
	      smart_bin(M, F, A, Lb, E);
	 (E) ->
	      E
      end,
      Form
     );
form(M, Form) ->
    Form.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Resolve local calls to primitive operations as follows:
%% 1. Collect all function names defined in module, including imports
%% 2. Walk each form, replacing local calls with remote ones
%%
%% As a side effect, we also resolve imported functions into remote
%% calls at this point.

simple_resolve_imports(Forms) ->
    Imps_and_funcs = func_defs(Forms),
    resolve_calls(Forms, Imps_and_funcs).

func_defs([{attribute, La, import, {M, FAs}}|Forms]) ->
    [ {{F,A}, {import, M}} || {F,A} <- FAs ] ++ func_defs(Forms);
func_defs([{function,Lf, F, A, Clss}|Forms]) ->
    [ {{F,A}, local} | func_defs(Forms) ];
func_defs([_|Forms]) ->
    func_defs(Forms);
func_defs([]) ->
    [].

resolve_calls(Forms, FuncDefs) ->
    [ resolve_form(Form, FuncDefs) || Form <- Forms ].

%% Resolve imports to remote calls. Note that we FIRST check whether
%% a local is a call to an erlang BIF. If so, the call is made to
%% erlang:f(...). This is deliberate, since Erlang itself behaves that way.
%% (Doing so also makes a mockery of scoping.)

resolve_form({function, _Lf, Fn, Ar, _Clss} = Form, FuncDefs) ->
    mapform0(
      fun({call, Lc, {atom, Lf, F}, As}=Expr) ->
	      FA = {F, A = length(As)},
	      case erlang:is_builtin(erlang,F,A) of
		  true ->
		      %% if ALSO defined locally, should warn
		      %% ?msg("Looking up ~p -> bif\n", [FA]),
		      Lm = Lc,
		      mk_remote_call(erlang, F, As);
		  false ->
		      case lists:keysearch(FA, 1, FuncDefs) of
			  {value, {_, local}} ->
			      %% ?msg("Looking up ~p -> local\n", [FA]),
			      Expr;
			  {value, {_, {import, M}}} ->
			      %% ?msg("Looking up ~p -> import\n", [FA]),
			      mk_remote_call(M, F, As);
			  false ->
			      %% ?msg("Looking up ~p -> undefined\n", [FA]),
			      Expr
		      end
	      end;
	 (Syntax) ->
	      Syntax
      end,
      Form
     );
resolve_form(Form, FuncDefs) ->
    Form.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% [R12B4]

%% Exprs = [Expr]
%% Cases = [Clause]
%% Exn_handlers = [Exn_clause]
%% After = [Expr]
%%
%% The Exn_handler clause looks like
%%   {clause,_,[{atom,_,Type},Pat,{var,_,'_'}], Guard, Body}
%% where Type = exit | error | throw

%% Here is a simplistic handler. The main problem is, Expr does
%% not export vars. I think. (Uhh, the docs sorta suggested it.)
%%
%% What we want to do is convert this macro into rewrite rules
%% that we can apply to all the relevant locations.

mk_try(Exprs, Cases, Exn_handlers, After) ->
    {'try', -1, Exprs, Cases, Exn_handlers, After}.

%% Type of exception is the atom Type.
%% The exception reason is caught as Rsn.
%% Body is what we do with it (should involve Rsn).

exn_handler(Type, Rsn, Body) ->
    {clause, -1, 
     [{tuple, -1, [{atom, -1, Type}, Rsn, {var, -1, '_'}]}],
     [],
     Body}.

mk_exit(Term) ->
    mk_nonlocal(exit, Term).

mk_error(Term) ->
    mk_nonlocal(error, Term).

mk_nonlocal(Ty, Term) ->
    mk_remote_call(erlang, Ty, [Term]).

%% P = E becomes
%% (case E of P=X -> X; _ -> P = exit(...))
%% [the P = exit(...) flourish is needed to fool the compiler rules
%%  for exporting variables]

smart_match(M, F, A, Line, Pat, Expr) ->
    X = new_var(),
    AbsMatch = {tuple, -1, [{atom, -1, match}, X]},
    {'case', -1, Expr,
     [{clause, -1, [{match,-1,Pat,X}], [], [X]},
      {clause, -1, [X], [], 
       [{match, -1, Pat, smart_error(M, F, A, Line, AbsMatch)}]}]}.

%% old alt: extract free vars and match each of them
%%    FVs = vars_of(Pat),
%%        [ {match, -1, Y, {atom, -1, nyi}} || Y <- FVs ]}]}.

%% M, F, A, Line, Rsn are concrete; Expr and Clss are abstract
%% Note: the Expr is already rewritten
%%
%% Add extra clause
%%   X -> exit({{M,F,A},{line, L}, {case_clause, X}})

smart_case(M, F, A, Line, Expr, Clss) ->
    X = new_var(),
    Case_term = {tuple, -1, [{atom,-1,case_clause}, X]},
    Term = exn_term({M, F, A}, {line, Line}, Case_term),
    {'case', -1, Expr,
     Clss ++
     [{clause, -1, [X], [], [mk_error(Term)]}]}.

%% M, F, A, Line, Rsn are concrete; Expr and Clss are abstract
%% Note: the Expr is already rewritten
%%
%% Add extra clause: 
%%    true -> exit({{M,F,A},{line,Line},if_clause})

smart_if(M, F, A, Line, Clss) ->
    Term = erl_parse:abstract({{M,F,A}, {line, Line}, if_clause}),
    {'if', -1,
     Clss ++
     [{clause, -1, [], [],
       [mk_error(Term)]}]}.

%% fun(P1,...,Pk) -> B end
%% add extra clause:
%%    (X1,...,Xn) -> exit({{M,F,A},{line,L},{fun_clause,X1,...,Xn}})

smart_fun(M, F, A, Line, Clss) ->
    Arity = clauses_arity(Clss),
    Xs = new_vars(Arity),
    Fun_args = {tuple, -1, [{atom, -1, fun_clause}] ++ Xs},
    Term = exn_term({M, F, A}, {line, Line}, Fun_args),
    {'fun', -1,
     {clauses, 
      Clss ++ [{clause, -1, Xs, [], [mk_error(Term)]}]}}.

%% Same as above, but preserves Info field too. This field is only added
%% internally, after sys_pre_expand.
%%
%% UNFINISHED
%% - is the Info field properly preserved? e.g., we're adding variables

smart_fun(M, F, A, Line, Clss, Info) ->
    Arity = clauses_arity(Clss),
    Xs = new_vars(Arity),
    Fun_args = {tuple, -1, [{atom, -1, fun_clause}, cons_list(Xs)]},
    Term = exn_term({M, F, A}, {line, Line}, Fun_args),
    {'fun', -1,
     {clauses,
      Clss ++ [{clause, -1, Xs, [], [mk_error(Term)]}]}, 
     Info}.

%% F(P1,...,Pk) -> B end
%% add extra clause:
%%    (X1,...,Xn) -> exit({{M,F,A},{line,L},{fun_clause,X1,...,Xn}})

smart_function(M, F, A, Line, Clss) ->
    Arity = clauses_arity(Clss),
    Xs = new_vars(Arity),
    Fun_args = {tuple, -1, [{atom, -1, function_clause}, cons_list(Xs)]},
    Term = exn_term({M, F, A}, {line, Line}, Fun_args),
    {function, Line, F, A,
     Clss ++
     [{clause, -1, Xs, [],
       [mk_error(Term)]}]}.

%% M, F, A, Line, Rsn are concrete; F and [A1,..,An] are abstract
%% A call F(E1,..,En) generates {{M,F,A},{line,L},[E1,..,En],Rsn}
%%
%% Rewrite to
%%   X1 = E1, ..., Xn = En,
%%   try F(X1,...,Xn)
%%   catch
%%         error:Rsn -> error({{M,F,A},{line,L},{bif, F, Xs}})
%%   end
%%
%% UNFINISHED - 
%% - format of returned Rsn

smart_bif(M, F, A, Line, Mod, Func, Arity, Args) ->
    Xs = new_vars(Arity),
    Evals = [ {match, -1, X, Arg} || {X, Arg} <- lists:zip(Xs, Args) ],
    Rsn = new_var(),
    %% currently returns {{bif, M, F, [X1,...,Xn]}, Rsn}
    %% should it be {bif, {M, F, [X1,...,Xn]}, Rsn}? something else?
    Bif = {tuple, -1,
	   [{tuple, -1, [{atom, -1, bif}, 
			 {atom, -1, Mod}, 
			 {atom, -1, Func}, 
			 cons_list(Xs)]},
	    {tuple, -1, [{atom, -1, reason}, Rsn]}]},
    Exn_term = exn_term({M, F, A}, {line, Line}, Bif),
    {block, -1,
     Evals ++
     [mk_try([mk_remote_call(Mod, Func, Xs)],
	     [],
	     %% [exn_handler(exit,  Rsn, [mk_exit(Exn_term)]),
	     [exn_handler(error, Rsn, [mk_error(Exn_term)])],
	     [])]}.

mk_remote_call(M, F, Xs) ->
    {call, -1, {remote, -1, {atom, -1, M}, {atom, -1, F}}, Xs}.

%% Rewrite to
%%   X1 = E1, X2 = E2,
%%   try X1 Binop X2 
%%   catch 
%%         error:Rsn -> error({{M,F,A},{line,L},{Binop, X1, X2}})
%%   end
%%
%% UNFINISHED
%% - Exn_term

smart_binop(M, F, A, Line, Op, E1, E2) ->
    X1 = new_var(),
    X2 = new_var(),
    Rsn = new_var(),
    Exn_rsn = {tuple, -1,
	       [{atom, -1, binop}, {atom, -1, Op}, cons_list([X1, X2]), 
		{tuple, -1, [{atom, -1, reason}, Rsn]}]},
    Exn_term = exn_term({M, F, A}, {line, Line}, Exn_rsn),
    {block, -1,
     [{match, -1, X1, E1},
      {match, -1, X2, E2},
      mk_try([mk_binop(Op, X1, X2)], [],
	     %% [exn_handler(exit,  Rsn, [mk_exit(Exn_term)]),
	     [exn_handler(error, Rsn, [mk_error(Exn_term)])],
	     [])
      ]}.

mk_binop(Op, X1, X2) ->
    {op, -1, Op, X1, X2}.

mk_unop(Op, X1) ->
    {op, -1, Op, X1}.

%% Rewrite to
%%   X1 = E1, 
%%   try Unop(X1)
%%   catch 
%%         error:Rsn -> error({{M,F,A},{line,L},{Unop, X1}})
%%   end
%%
%% UNFINISHED
%% - we assume the unop generates an error if it fails

smart_unop(M, F, A, Line, Op, Expr) ->
    X = new_var(),
    Rsn = new_var(),
    Exn_rsn = {tuple, -1,
	       [{atom, -1, unop}, {atom, -1, Op}, cons_list([X]), 
		{tuple, -1, [{atom, -1, reason}, Rsn]}]},
    Exn_term = exn_term({M, F, A}, {line, Line}, Exn_rsn),
    {block, -1,
     [{match, -1, X, Expr},
      mk_try([mk_unop(Op, X)], [],
	     %% [exn_handler(exit,  Rsn, [mk_exit(Exn_term)]),
	     [exn_handler(error, Rsn, [mk_error(Exn_term)])],
	     [])
      ]}.

%% Rewrite to exit({{M,F,A},{line, L}, AbsRsn})
%%   where AbsRsn is already abstracted

smart_exit(M, F, A, Line, AbsRsn) ->
    Term = exn_term({M, F, A}, {line, Line}, AbsRsn),
    mk_remote_call(erlang, exit, [Term]).

%% Rewrite to fault({{M,F,A},{line, L}, Rsn})

smart_fault(M, F, A, Line, AbsRsn) ->
    Term = exn_term({M, F, A}, {line, Line}, AbsRsn),
    mk_remote_call(erlang, fault, [Term]).

%% Rewrite to error({{M,F,A},{line, L}, Rsn})

smart_error(M, F, A, Line, AbsRsn) ->
    Term = exn_term({M, F, A}, {line, Line}, AbsRsn),
    mk_remote_call(erlang, error, [Term]).

%% Rewrite into a tuple where all args except last (AbsT, Abs) are
%% concrete terms. AbsT, Abs are abstract terms.

exn_term(T1, T2, AbsT) ->
    exn_tuple([T1, T2], AbsT).

exn_tuple(Concs, Abs) ->
    {tuple, -1, [ erl_parse:abstract(Conc) || Conc <- Concs ] ++ [Abs]}.

%% Rewrite a binop into something that catches errors and indicates
%% the fault. Since binary expressions may be quite large (e.g., a
%% dozen variables), we should generate something informative as well
%% as indicate the location.
%%
%% UNFINISHED
%% - we could use the element type specs to ferret out which values are
%%   inappropriate (one or more) and just return those
%%   * something similar was optionally done in version 1.0 for BIFs
%%   * left for "future work"

smart_bin(M, F, A, Line, {bin, _Lb, BinElts}=Expr) ->
    Rsn = new_var(),
    Exn_rsn = bin_error(Expr, Rsn),
    Exn_term = exn_term({M, F, A}, {line, Line}, Exn_rsn),
    mk_try([Expr], [],
	   %% [exn_handler(exit,  Rsn, [mk_exit(Exn_term)]),
	   [exn_handler(error, Rsn, [mk_error(Exn_term)])],
	   []).


bin_error({bin, Lb, BinElts}=Expr, AbsRsn) ->
    %% io:format("%% decorating ~w\n", [Expr]),
    {tuple, -1,
     lists:flatten(
       [ bin_indicator(P, Lbe, Type, Width)
	 || {bin_element, Lbe, P, Type, Width} <- BinElts ]) 
     ++ [{tuple, -1, [{atom, -1, reason}, AbsRsn]}]}.

%% Emit variable name, value and expected type/width for all elements.
%% Note that the Type and Width arguments are a bit underspecified,
%% so we basically emit them at will and hope they are reasonably
%% legible. [Improve as needed.]

bin_indicator({var, Lx, X}=Var, Line, Width, Type) when X =/= '_' ->
    exn_tuple([X, bin_type_summary(Width, Type)], Var);
bin_indicator({integer, _Lc, _C}, _Line, _Type, _Width) ->
    [];
bin_indicator(P, Line, Type, Width) ->
    io:format("%% bin_indicator: skipped unknown pattern"
	      " ~w on line ~w\n", [P, Line]),
    [].

%% The default values are just guessed at this point
%% - assume the default type to be unsigned-big
%% - assume default width to be 8 bits (byte)
%%
%% UNFINISHED
%% - short widths do not require endianness; we could
%%   emit a more concise type in this case
%%   * could also just skip endianness; sign + size sufficient for debug?

bin_type_summary(default, [binary]) ->
    binary;
bin_type_summary({integer, Line, Width}, [binary]) ->
    {binary, Width};
bin_type_summary({integer, _Line, Width}, default) ->
    {default_bin_type(), Width};
bin_type_summary({integer, _Line, Width}, Type) ->
    {Type, Width};
bin_type_summary(default, default) ->
    resolve_default_width({default_bin_type(), default});
bin_type_summary(default, TypeSegs) ->
    resolve_default_width(
      lists:foldr(
	fun({Last, Width}, unknown) ->
		%% must be final item or we die
		{[Last], Width};
	   (Last, unknown) ->
		%% must be final item or we die
		{[Last], default};
	   (Seg, {Segs, Width}) ->
		{[Seg|Segs], Width}
	end,
	unknown,
	TypeSegs));
bin_type_summary(Type, W) ->
    io:format("%% unknown type summarized as self: ~w\n", [Type]),
    {Type, W}.

%% UNFINISHED
%% - an educated guess that this is the default type

default_bin_type() ->
    [unsigned,big].

%% Resolve default type into a width appropriate for the code
%% - we ASSUME that Type is an integer of some sort, where default
%%   is ASSUMED to be 8
%% (- binaries never occur, since they are resolved beforehand in
%%    bin_type_summary/2)

resolve_default_width({Type, default}) ->
    {Type, 8};
resolve_default_width(TypeWidth) ->
    TypeWidth.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clauses_arity([{clause, _, H, G, B}|_]) ->
    length(H).

%%

new_vars(N) ->
    new_vars(1,N).

new_vars(M,N) ->
    if
	M > N ->
	    [];
	true ->
	    [new_var()|new_vars(M+1,N)]
    end.

%%

new_var() ->
    K = counter('exc var counter'),
    {var,0,list_to_atom("_" ++ integer_to_list(K))}.

%% from ap_util.erl

counter(Name) ->
    Ix =
	case get(Name) of
	    N when is_integer(N) ->
		N;
	    undefined ->
		0
	end,
    put(Name,Ix+1),
    Ix.

%% UNFINISHED
%% - parametrized module should pass the parameter values, but
%%   currently passes only parameter names (as atoms)
%%   - introducing this may need some rewriting of sites where module name
%%     is used, because of the use of vars vs. abstraction of ground terms

get_module_name([{attribute,Lm,module,Mod}|Xs]) ->
    Mod;
%get_module_name([{attribute,Lm,module,Mod}|Xs]) ->
%    case Mod of
%	M when atom(M) ->
%	    M;
%	{M, Xs} when atom(M) ->
%	    M
%    end;
get_module_name([_|Xs]) ->
    get_module_name(Xs).

%%

function_name({function, Lf, F, A, Clss}) ->
    {F, A};
function_name(Other) ->
    {not_a_function, no_name}.

%%

zip([X|Xs], [Y|Ys]) ->
    [{X,Y} | zip(Xs, Ys)];
zip([], []) ->
    [].

%%

cons_list([X|Xs]) ->
    {cons, 0, X, cons_list(Xs)};
cons_list([]) ->
    {nil, 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Simple version of mapform.erl
%%
%% NOTES
%% - constant/1 has apparently been deprecated by some fool, thus
%%    atom ; number

mapform0(F, {clause, Lc, H, G, B}) ->
    F({clause, Lc, H, G, mapform0(F, B)});
mapform0(F, {match, Lc, P, E}) ->
    F({match, Lc, P, mapform0(F, E)});
mapform0(F, {lc, Llc, E, GQs}) ->
    F({lc, Llc, mapform0(F, E), [ mapform1(F, GQ) || GQ <- GQs ]});
mapform0(F, T) when is_tuple(T) ->
    F(list_to_tuple([ mapform0(F, Tsub) || Tsub <- tuple_to_list(T) ]));
mapform0(F, Xs) when is_list(Xs) ->
    [ mapform0(F, X) || X <- Xs ];
mapform0(F, C) when is_atom(C) ; is_number(C) ->
    C.

%% detect + elide pattern in qualifier

mapform1(F, {generate, Lg, P, E}) ->
    {generate, Lg, P, mapform0(F, E)};
mapform1(F, Qual) ->
    mapform0(F, Qual).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Compute the variables occurring in the term.
%%  Variables are abstract vars, {var, _, X} where X is not '_'
%%  while Term can be any term (possibly containing some variables
%%  per above), but is probably an abstract syntax tree of some sort.
%%
%% Note: No handling of scoping, etc.
%%
%% This is just a heuristic to make the erlang compiler shut up. (Perhaps
%% not needed anymore?)

vars_of(AbsTerm) ->
    FVs0 = vars_of(AbsTerm, []),
    FVs = sets:to_list(sets:from_list(FVs0)),
    FVs.

vars_of({var, _, '_'}, Vs) ->
    Vs;
vars_of({var, _, X}=V, Vs) ->
    [V|Vs];
vars_of(T, Vs0) when is_tuple(T) ->
    lists:foldl(
      fun(Term, Vs) ->
	      vars_of(Term, Vs)
      end,
      Vs0,
      tuple_to_list(T));
vars_of([X|Xs], Vs) ->
    vars_of(Xs, vars_of(X, Vs));
vars_of([], Vs) ->
    Vs;
vars_of(X, Vs) ->
    %% cannot be a variable or contain a variable
    Vs.

