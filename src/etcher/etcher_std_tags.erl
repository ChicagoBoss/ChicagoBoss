%%%-------------------------------------------------------------------
%%% File    : etcher_std_tags.erl
%%% Project : Etcher (http://github.com/jinsky/etcher)
%%% Author  : Rory Byrne <rory [at] jinsky [dot] com>
%%% License : BSD
%%%
%%% Copyright (c) 2010 Rory Byrne
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   * Redistributions of source code must retain the above copyright
%%%     notice, this list of conditions and the following disclaimer.
%%%
%%%   * Redistributions in binary form must reproduce the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer in the documentation and/or other materials provided
%%%     with the distribution.
%%%
%%%   * Neither the names of the copyright holders, nor the names of its
%%%     contributors may be used to endorse or promote products derived
%%%     from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(etcher_std_tags).

%% Tags still TODO:
%%   - load

-export([standard_tags/0,
         tag_autoescape/2, render_autoescape/2,
         tag_block/2, render_block/2,
         tag_comment/2, 
         tag_csrf_token/2, 
         tag_cycle/2, render_cycle/2,
         tag_debug/2, render_debug/2,
         ftag_extends/2, render_extends/2,
         tag_filter/2, render_filter/2,
         tag_firstof/2, render_firstof/2,
         tag_for/2, render_for/2,
         tag_if/2, render_if/2,
         tag_ifchanged/2, render_ifchanged/2,
         tag_ifequal/2, render_ifequal/2,
         tag_ifnotequal/2, render_ifnotequal/2,
         tag_include/2, render_include/2,
         tag_now/2, render_now/2,
         tag_regroup/2, render_regroup/2,
         tag_spaceless/2, render_spaceless/2,
         tag_ssi/2, render_ssi/2,
         tag_templatetag/2, render_templatetag/2,
         tag_url/2, render_url/2,
         tag_widthratio/2, render_widthratio/2,
         tag_with/2, render_with/2
         ]).

-include("internal.hrl").

-record(forloop, {
            counter,
            counter0,
            revcounter,
            revcounter0,
            first,
            last,
            parentloop
            }).

-record(loop_spec, {
            var_names, 
            source_var,
            reversed,
            main_block,
            ifempty_block
            }).

-define(BLOCKS_KEY, "_blocks").

standard_tags() ->
    FNames = [atom_to_list(F) || {exports, Exports} <- module_info(),
                                             {F, 2} <- Exports],
    L = [new_tag_def(Name, list_to_atom(F)) || F = "tag_" ++ Name <- FNames],
    [new_extends_tag_def() | L].

new_tag_def(Name, Function) ->
    #tag_def{
        name = Name, 
        mf = {?MODULE, Function}}.

% The extends tag is special because it needs to be the first tag in a 
% template.
new_extends_tag_def() ->
    TagDef = new_tag_def("extends", ftag_extends),
    TagDef#tag_def{must_be_first=true}.

%%------------------------------------------------------------------------
%% Tag: autoescape
%%------------------------------------------------------------------------

tag_autoescape(PS, #tag{extra=Arg}) ->
    Bool =
        case Arg of
            "on" ->
                true;
            "off" ->
                false;
            "" ->
                ErrStr = "'Autoescape' tag requires exactly one argument.",
                invalid_syntax(ErrStr);
            _ ->
                ErrStr = "'Autoescape' argument should be 'on' or 'off'",
                invalid_syntax(ErrStr)
        end, 
    {Parsed, PS1} = parse_until(PS, "endautoescape"),
    {{?MODULE, render_autoescape, {Parsed, Bool}}, PS1}.

render_autoescape(RS, {Underlings, OnOff}) ->
    RS1 = RS#rs{auto_escape=OnOff},
    render(RS1, Underlings).

%%------------------------------------------------------------------------
%% Tag: block - (Tightly coupled to 'extends' tag)
%%------------------------------------------------------------------------

% Note - this function is called directly from extends tag. Use caution
%        if changing return type.
tag_block(PS, #tag{extra=S}) ->
    BlockName = get_block_name(S),
    {Parsed, PS1} = parse_until_endblock(PS, BlockName),
    {{?MODULE, render_block, {Parsed, BlockName}}, PS1}.

parse_until_endblock(PS, BlockName) ->
    case etcher_parser:parse_until(PS, ["endblock"]) of
        {#tag{name="endblock", extra=EndTagParam}, Parsed, PS1} when
                            EndTagParam =:= ""; EndTagParam =:= BlockName ->
            {Parsed, PS1};
        {#tag{extra=Unexpected}, _Parsed, _PS1} ->
            ErrStr = "Bad end tag content for block named '" ++ 
                            BlockName ++ "': '" ++ Unexpected ++ "'",
            invalid_syntax(ErrStr)
    end.

get_block_name(S) ->
    case string_split(S) of
        [Name] when Name =/= "" ->
            Name;
        _ ->
            ErrStr = "'block' tag requires a single name argument",
            invalid_syntax(ErrStr)
    end.        

render_block(#rs{context=Context} = RS, {Underlings, BlockName}) ->
    Blocks = proplists:get_value(?BLOCKS_KEY, Context, []),
    {_NewRS, Default} = render(RS, Underlings),
    RS1 = set_super_block(Default, RS),
    render_block(Blocks, BlockName, RS1, Default).

render_block([{BlockName, Underlings} | Rest], BlockName, RS, _Current) ->
    {_NewRS, NewCurrent} = render(RS, Underlings),
    RS1 = set_super_block(NewCurrent, RS),
    render_block(Rest, BlockName, RS1, NewCurrent);
render_block([_ | Rest], BlockName, RS, Current) ->
    render_block(Rest, BlockName, RS, Current);
render_block([], _BlockName, _RS, Current) ->
    Current.

set_super_block(Content, RS) ->
    S = unicode:characters_to_list(Content),
    String = etcher_util:to_string(S),
    String1 = String#string{safe=true},
    update_context("block", [{"super", String1}], RS).

%%------------------------------------------------------------------------
%% Tag: comment
%%------------------------------------------------------------------------

tag_comment(PS, _Tag) ->
    _PS1 = etcher_parser:skip_until(PS, "endcomment").

%%------------------------------------------------------------------------
%% Tag: csrf_token
%%------------------------------------------------------------------------

% Note: This is a no-op tag in Django 1.0. It's just a placeholder.
tag_csrf_token(PS, _Tag) ->
    PS.

%%------------------------------------------------------------------------
%% Tag: cycle
%%------------------------------------------------------------------------

% Note: Cycles should behave the same way as in Django, with one exception:
%       Named Cycles aren't checked at compile time, and if a non-existant
%       named cycle is called, it will just return an empty string at 
%       runtime.
tag_cycle(PS, #tag{extra=S}) ->
    CycleCmd = parse_cycle_cmd(S, PS),
    {{?MODULE, render_cycle, CycleCmd}, PS}.

parse_cycle_cmd("", _PS) ->
    invalid_cycle_syntax();    
parse_cycle_cmd(S, PS) ->
    [FirstTok | _] = Toks = string_split(S),
    case lists:member($,, FirstTok) of
        true ->            
            parse_old_style_cycle(Toks, PS);
        false when length(Toks) =:= 1 ->
            {call_named_cycle, FirstTok};
        false ->
            parse_new_style_cycle(Toks, PS)
    end.
            
parse_old_style_cycle([FirstTok | Rest], PS) ->
    CommaSepToks = string_split(FirstTok, ",", list),
    L = [lists:flatten([$", S, $"]) || S <- CommaSepToks],
    parse_new_style_cycle(L ++ Rest, PS).

parse_new_style_cycle(Toks, PS) ->
    case lists:split(length(Toks) - 2, Toks) of
        {Toks1, ["as", CycleName]} when length(Toks1) > 0 ->
            Vars = to_variables(PS, Toks1),
            {cycle, named, Vars, CycleName};
        _ ->
            Vars = to_variables(PS, Toks),
            {cycle, dynamic, Vars, unique_key()}
    end.

invalid_cycle_syntax() ->
    invalid_syntax("'cycle' tag is incorrect, please check syntax").

render_cycle(RS, {call_named_cycle, CycleName}) ->
    Cycles = get_cycles(named, RS),
    case proplists:lookup(CycleName, Cycles) of
        {CycleName, Vals} ->
            do_cycle(named, Vals, CycleName, RS);
        _ ->
            ""
    end;
render_cycle(RS, {cycle, named, Vars, CycleName}) ->
    init_cycle(named, Vars, CycleName, RS);
render_cycle(RS, {cycle, dynamic, Vars, Key}) ->
    Cycles = get_cycles(dynamic, RS),
    case proplists:lookup(Key, Cycles) of
        {Key, Vals} ->
            do_cycle(dynamic, Vals, Key, RS);
        none ->
            init_cycle(dynamic, Vars, Key, RS)
    end.

init_cycle(Type, Vars, Key, RS) ->
    Vals = [resolve_variable(RS, V) || V <- Vars],
    do_cycle(Type, Vals, Key, RS).

do_cycle(Type, Vals, Key, RS) ->
    [Item | Rest] = Vals,
    Vals1 = Rest ++ [Item],
    RS1 = update_cycles(Type, Key, Vals1, RS),
    {RS1, Item}.

update_cycles(Type, CycleName, Vals, RS) ->
    Cycles = get_cycles(Type, RS),
    Cycles1 = [{CycleName, Vals} | proplists:delete(CycleName, Cycles)],
    GlobalKey = global_cycle_key(Type),
    update_globals(GlobalKey, Cycles1, RS).

get_cycles(Type, #rs{globals=Globals}) ->
    GlobalKey = global_cycle_key(Type),
    proplists:get_value(GlobalKey, Globals, []).

global_cycle_key(named) ->
    "cycle.named_cycles";
global_cycle_key(dynamic) ->
    "cycle.dynamic_cycles".

%%------------------------------------------------------------------------
%% Tag: debug
%%------------------------------------------------------------------------

tag_debug(PS, _Tag) ->
    {{?MODULE, render_debug, []}, PS}.

render_debug(#rs{context=Context}, []) ->
    Fmt = "\n---- DEBUG ----\n"
          "Context: ~p\n" 
          "\n---- END DEBUG ----\n",
    io_lib:format(Fmt, [Context]).

%%------------------------------------------------------------------------
%% Tag: extends - (Tightly coupled to 'block' tag)
%%------------------------------------------------------------------------

% Different from other tags in that it must appear first in a template - 
% hence the 'ftag_' prefix.
ftag_extends(PS, #tag{extra=S}) ->
    SuperTplVar = compile_variable(PS, S),
    {NamedBlocks, PS1} = parse_blocks(PS, []),
    Args = {SuperTplVar, NamedBlocks},
    {{?MODULE, render_extends, Args}, PS1}.

parse_blocks(#ps{tokens=[#tag{name="block"} = Tag | Rest]} = PS, NamedBlocks) ->
    PS1 = PS#ps{tokens=Rest},
    {{_Mod, _Fun, {Underlings, BlockName}}, _NewPS} = tag_block(PS1, Tag),
    case proplists:is_defined(BlockName, NamedBlocks) of
        true ->
            ErrStr = "'block' tag with name '" ++ BlockName ++ 
                            "' appears more than once",
            invalid_syntax(ErrStr);
        false ->
            NamedBlocks1 = [{BlockName, Underlings} | NamedBlocks],
            parse_blocks(PS1, NamedBlocks1)
    end;
parse_blocks(#ps{tokens=[_ | Rest]} = PS, NamedBlocks) ->
    parse_blocks(PS#ps{tokens=Rest}, NamedBlocks);
parse_blocks(#ps{tokens=[]} = PS, NamedBlocks) ->
    {lists:reverse(NamedBlocks), PS}.

render_extends(RS, {SuperTplName, NamedBlocks}) ->
    case find_template(SuperTplName, RS) of
        undefined ->
            throw({bad_extended_template, SuperTplName}); 
        #etcher_template{} = Template ->
            extend_template(Template, NamedBlocks, RS)
    end.

find_template(SuperTplName, RS) ->
    case resolve_variable(RS, SuperTplName) of
        #etcher_template{} = Tpl ->
            Tpl;
        FilePath when ?IS_STRING(FilePath) ->
            etcher_loader:get_template(RS, FilePath);
        _ ->
            undefined
    end.

extend_template(Template, NamedBlocks, #rs{context=Context} = RS) ->
    Blocks = proplists:get_value(?BLOCKS_KEY, Context, []),
    Blocks1 = NamedBlocks ++ Blocks,
    RS1 = update_context(?BLOCKS_KEY, Blocks1, RS),
    {_NewRS, Rendered} = etcher_renderer:render_template(RS1, Template),
    {RS, Rendered}.                                 % Return orignal #rs{}

%%------------------------------------------------------------------------
%% Tag: filter
%%------------------------------------------------------------------------

tag_filter(PS, #tag{extra=S}) ->
    Var = parse_filter_pipeline(S, PS),
    {Parsed, PS1} = parse_until(PS, "endfilter"),
    {{?MODULE, render_filter, {Parsed, Var}}, PS1}.

parse_filter_pipeline("", _PS) ->
    invalid_syntax("'filter' tag pipeline must contain at least one filter");
parse_filter_pipeline(S, PS) ->
    S1 = "placeholder|" ++ S,
    Var = compile_variable(PS, S1),
    check_for_escape_filters(Var#variable.filters),
    Var.

check_for_escape_filters([#filter{name=Name} | _]) when Name =:= "escape"; 
                                                        Name =:= "safe" ->
    ErrStr = "'filter " ++ Name ++ "' is not permitted. "
                    "Use the 'autoescape' tag instead.",
    invalid_syntax(ErrStr);
check_for_escape_filters([_ | Rest]) ->
    check_for_escape_filters(Rest);
check_for_escape_filters([]) ->
    ok.

render_filter(RS, {Underlings, Var}) ->
    {RS1, CharData} = render(RS, Underlings),
    CharList = unicode:characters_to_list(CharData),
    String = #string{val=CharList},
    Var1 = Var#variable{val=String},
    S = render_variable(RS, Var1),
    {RS1, S}.

%%------------------------------------------------------------------------
%% Tag: firstof
%%------------------------------------------------------------------------

tag_firstof(PS, #tag{extra=S}) ->
    case string_split(S) of
        [] ->
            ErrStr = "'firstof' statement requires at least one argument",
            invalid_syntax(ErrStr);
        L ->
            Vars = to_variables(PS, L),
            {{?MODULE, render_firstof, Vars}, PS}
    end.

render_firstof(RS, Vars) ->
    firstof(Vars, RS).

firstof([Var | Rest], RS) ->
    case resolve_variable(RS, Var) of
        Value when ?EVALS_TO_FALSE(Value) ->
            firstof(Rest, RS);
        Value ->
            stringify(Value)
    end;
firstof([], _RS) ->
    "".

%%------------------------------------------------------------------------
%% Tag: for
%%------------------------------------------------------------------------

tag_for(PS, #tag{extra=S}) ->
    LoopSpec = parse_for_statement(S, PS),
    {MainBlock, IfEmptyBlock, PS1} =
        case parse_until(PS, ["empty", "endfor"]) of
            {"empty", MainParsed, P1} ->
                {IfEmptyParsed, P2} = parse_until(P1, "endfor"),
                {MainParsed, IfEmptyParsed, P2};
            {"endfor", MainParsed, P1} ->
                {MainParsed, "", P1}
        end,
    LoopSpec1 = LoopSpec#loop_spec{
                            main_block = MainBlock, 
                            ifempty_block = IfEmptyBlock},
    {{?MODULE, render_for, LoopSpec1}, PS1}.

% Note: Django splits on whitespace. This can give rise to problems, such
%       as when the source list is a string literal containing spaces,
%       eg {% for letter in "world wide web" %}.
%       We follow in Django's footsteps to maintain compatibility.
parse_for_statement(S, PS) ->
    try 
        Tokens = split_for_tokens(S),
        {SourceVar, IsReversed, Tokens1} = parse_source_var(Tokens, PS),
        VarNames = split_var_names(Tokens1),
        #loop_spec{
            var_names = VarNames, 
            source_var = SourceVar, 
            reversed = IsReversed}
    catch
        throw:invalid_syntax ->
            ErrStr = "'for' statements should use the format "
                    "'for x in y': " ++ S,
            invalid_syntax(ErrStr)
    end.

split_for_tokens(S) ->
    case string_split(S) of
        Toks when length(Toks) >= 3 ->
            Toks;
        _ ->
            throw(invalid_syntax)
    end.

parse_source_var(Toks, PS) ->
    case lists:split(length(Toks) - 3, Toks) of
        {_, [_, "in", "reversed"]} ->
            throw(invalid_syntax);
        {Toks1, ["in", S, "reversed"]} ->
            SourceVar = compile_variable(PS, S),
            {SourceVar, true, Toks1};
        {Toks1, [V, "in", S]} ->
            Toks2 = Toks1 ++ [V],
            SourceVar = compile_variable(PS, S),
            {SourceVar, false, Toks2};
        _ ->
            throw(invalid_syntax)
    end.

split_var_names(Tokens) ->
    S = string:join(Tokens, " "),
    VarNames = string_split(S, "\\s*,\\s*", list),
    HasWhitespace = fun(Word) -> lists:member($\s, Word) end,
    case lists:any(HasWhitespace, VarNames) of
        true ->
            throw(invalid_syntax);
        false ->
            VarNames
    end.

render_for(RS, #loop_spec{source_var=SourceVar,
                          var_names=VarNames,
                          main_block=MainBlock,
                          ifempty_block=IfEmptyBlock,
                          reversed=IsReversed}) ->
    case resolve_variable(RS, SourceVar) of
        [] ->
            render(RS, IfEmptyBlock);
        Items when is_list(Items) ->
            Items1 = 
                case IsReversed of
                    true ->
                        lists:reverse(Items);
                    false ->
                        Items
                end,
            loop(Items1, VarNames, MainBlock, RS);
        _ ->
            ""
    end.

loop(Items, VarNames, MainBlock, #rs{context=Context} = RS) ->
    ParentLoop = proplists:get_value("forloop", Context),
    Forloop = new_forloop(Items, ParentLoop),
    try loop(Items, VarNames, MainBlock, Forloop, RS, [])
    catch 
        throw:{forloop, _Reason} ->
            ""
    end.

loop([Item | Rest], VarNames, MainBlock, Forloop, RS, Acc) ->
    Vars = unpack_forloop_vars(VarNames, Item),
    Vars1 = [{"forloop", expand_record(Forloop)} | Vars],
    RS1 = update_context(Vars1, RS),
    {RS2, L} = render(RS1, MainBlock),
    Forloop1 = update_forloop(Forloop, Rest),
    loop(Rest, VarNames, MainBlock, Forloop1, RS2, [L | Acc]);
loop([], _VarNames, _MainBlock, _Forloop, _RS, Acc) ->
    lists:reverse(Acc).

new_forloop(Items, ParentLoop) ->
    #forloop{
        counter = 1,
        counter0 = 0,
        revcounter = length(Items),
        revcounter0 = length(Items) - 1,
        first = true,
        last = length(Items) =:= 1,
        parentloop = ParentLoop}.

update_forloop(#forloop{counter=Counter,
                        counter0=ZCounter,
                        revcounter=RevCounter,
                        revcounter0=ZRevCounter} = Forloop,
               ItemsLeft) ->
    Forloop#forloop{
        counter = Counter + 1,
        counter0 = ZCounter + 1,
        revcounter = RevCounter - 1,
        revcounter0 = ZRevCounter - 1,
        first = false,
        last = length(ItemsLeft) =:= 1}.

unpack_forloop_vars([VarName], Item) ->
    [{VarName, Item}];
unpack_forloop_vars(VarNames, Item) when is_tuple(Item) ->
    unpack_forloop_vars(VarNames, tuple_to_list(Item));
unpack_forloop_vars(VarNames, Items) when is_list(Items) ->
    unpack_varlist(VarNames, Items, []);
unpack_forloop_vars(VarNames, Item) ->
    throw({forloop, {cant_unpack_forloop_vars, VarNames, Item}}).

unpack_varlist([Name | RestNames], [Val | RestVals], Acc) ->
    Acc1 = [{Name, Val} | Acc],
    unpack_varlist(RestNames, RestVals, Acc1);
unpack_varlist([Name | RestNames], [], Acc) ->
    Acc1 = [{Name, ""} | Acc],
    unpack_varlist(RestNames, [], Acc1);
unpack_varlist([], _RestVals, Acc) ->
    Acc.

%%------------------------------------------------------------------------
%% Tag: if
%%------------------------------------------------------------------------

tag_if(PS, #tag{extra=S}) ->
    Condition = parse_if_statement(S, PS),
    {MainBlock, ElseBlock, PS1} =
        case parse_until(PS, ["else", "endif"]) of
            {"else", MainParsed, P1} ->
                {ElseParsed, P2} = parse_until(P1, "endif"),
                {MainParsed, ElseParsed, P2};
            {"endif", MainParsed, P1} ->
                {MainParsed, "", P1}
        end,
    {{?MODULE, render_if, {Condition, MainBlock, ElseBlock}}, PS1}.

parse_if_statement(S, PS) ->
    case string_split(S) of
        [] ->
            invalid_syntax("'if' statement requires at least one argument");
        L ->
            parse_if_var(L, PS, noop, [])
    end.

parse_if_var([], _PS, _Op, _Acc) ->
    ErrStr = "'if' statement ended unexpectedly - variable ref expected",
    invalid_syntax(ErrStr);
parse_if_var(["not", VarStr | Rest], PS, Op, Acc) ->
    Var = compile_if_var(VarStr, PS),
    parse_if_op(Rest, PS, Op, [{'NOT', Var} | Acc]);
parse_if_var([VarStr | Rest], PS, Op, Acc) ->
    Var = compile_if_var(VarStr, PS),
    parse_if_op(Rest, PS, Op, [Var | Acc]).

parse_if_op([], _PS, Op, Acc) ->
    Parts = lists:reverse(Acc),
    case Op of
        noop ->
            Parts;
        "and" ->
            ['AND' | Parts];
        "or" ->
            ['OR' | Parts]
    end;
parse_if_op([NewOp | Rest], PS, Op, Acc) when NewOp =:= "and"; NewOp =:= "or" ->
    case (NewOp =:= Op) or (Op =:= noop) of
        true ->
            parse_if_var(Rest, PS, NewOp, Acc);
        false ->
            invalid_syntax("'if' tags can't mix 'and' and 'or'")
    end;
parse_if_op([BadOp | _], _PS, _Op, _Acc) ->
    ErrStr = "'if' statement contains invalid logic operator: " ++ BadOp,
    invalid_syntax(ErrStr).

compile_if_var(Keyword, _PS) when (Keyword =:= "and") or 
                                  (Keyword =:= "or") or 
                                  (Keyword =:= "not") ->
    ErrStr = "The keywords 'and', 'or' and 'not' cannot appear as variable "
            "names in an 'if' tag statement",
    invalid_syntax(ErrStr);
compile_if_var(VarStr, PS) -> 
    compile_variable(PS, VarStr).

render_if(RS, {Condition, MainUnderlings, ElseUnderlings}) ->
    case eval_condition(Condition, RS) of
        true ->
            render(RS, MainUnderlings);
        false ->
            render(RS, ElseUnderlings)
    end.

eval_condition([Part], RS) ->
    is_true_condition(Part, RS);
eval_condition([Op | Parts], RS) ->
    Pred = fun(P) -> is_true_condition(P, RS) end,
    case Op of
        'AND' ->
            lists:all(Pred, Parts);
        'OR' ->
            lists:any(Pred, Parts)
    end.

is_true_condition({'NOT', Var}, RS) ->
    not is_true_condition(Var, RS);
is_true_condition(Var, RS) ->
    is_var_true(Var, RS).

%%------------------------------------------------------------------------
%% Tag: ifchanged
%%------------------------------------------------------------------------

tag_ifchanged(PS, #tag{extra=S}) ->
    Condition = parse_ifchanged_condition(S, PS),
    {MainBlock, ElseBlock, PS1} =
        case parse_until(PS, ["else", "endifchanged"]) of
            {"else", MainParsed, P1} ->
                {ElseParsed, P2} = parse_until(P1, "endifchanged"),
                {MainParsed, ElseParsed, P2};
            {"endifchanged", MainParsed, P1} ->
                {MainParsed, "", P1}
        end,
    {{?MODULE, render_ifchanged, {Condition, MainBlock, ElseBlock}}, PS1}.

parse_ifchanged_condition("", _PS) ->
    {unique_key(), self};
parse_ifchanged_condition(S, PS) ->
    L = string_split(S),
    [{unique_key(), compile_variable(PS, VarStr)} || VarStr <- L].

render_ifchanged(#rs{globals=Globals} = RS, 
                 {{Key, self}, MainBlock, ElseBlock}) ->
    {RS1, L} = render(RS, MainBlock),
    Hash = etcher_util:hash(L),
    case proplists:get_value(Key, Globals) of
        Hash ->
            render(RS, ElseBlock);
        _ ->
            RS2 = update_globals(Key, Hash, RS1),
            {RS2, L}
    end;
render_ifchanged(RS, {Condition, MainBlock, ElseBlock}) ->
    case ifchanged(Condition, RS) of
        {true, RS1} ->
            render(RS1, MainBlock);
        {false, _} ->
            render(RS, ElseBlock)
    end.
    
ifchanged(Condition, RS) ->
    ifchanged(Condition, RS, false).

ifchanged([{Key, VarStr} | Rest], #rs{globals=Globals} = RS, HasChanged) ->
    Var = resolve_variable(RS, VarStr),
    Hash = etcher_util:hash(Var),
    case proplists:lookup(Key, Globals) of
        {Key, Hash} ->
            ifchanged(Rest, RS, HasChanged);
        _ ->
            RS1 = update_globals(Key, Hash, RS),
            ifchanged(Rest, RS1, true)
    end;
ifchanged([], RS, HasChanged) ->
    {HasChanged, RS}.

%%------------------------------------------------------------------------
%% Tag: ifequal
%%------------------------------------------------------------------------

tag_ifequal(PS, #tag{extra=S}) ->
    case string_split(S) of
        [_, _] = L ->
            [Var1, Var2] = to_variables(PS, L),
            {MainBlock, ElseBlock, PS1} =
                case parse_until(PS, ["else", "endifequal"]) of
                    {"else", MainParsed, P1} ->
                        {ElseParsed, P2} = parse_until(P1, "endifequal"),
                        {MainParsed, ElseParsed, P2};
                    {"endifequal", MainParsed, P1} ->
                        {MainParsed, "", P1}
                end,
            {{?MODULE, render_ifequal, {MainBlock, ElseBlock, Var1, Var2}}, PS1};
        _ ->
            invalid_syntax("'ifequal' takes two arguments")
    end.

render_ifequal(RS, {MainBlock, ElseBlock, Var1, Var2}) ->
    case resolve_variable(RS, Var1) =:= resolve_variable(RS, Var2) of
        true ->
            render(RS, MainBlock);
        false ->
            render(RS, ElseBlock)
    end.

%%------------------------------------------------------------------------
%% Tag: ifnotequal
%%------------------------------------------------------------------------

tag_ifnotequal(PS, #tag{extra=S}) ->
    case string_split(S) of
        [_, _] = L ->
            [Var1, Var2] = to_variables(PS, L),
            {Parsed, PS1} = parse_until(PS, "endifnotequal"),
            {{?MODULE, render_ifnotequal, {Parsed, Var1, Var2}}, PS1};
        _ ->
            invalid_syntax("'ifnotequal' takes two arguments")
    end.

render_ifnotequal(RS, {Underlings, Var1, Var2}) ->
    case resolve_variable(RS, Var1) =/= resolve_variable(RS, Var2) of
        true ->
            render(RS, Underlings);
        false ->
            ""
    end.

%%------------------------------------------------------------------------
%% Tag: include
%%------------------------------------------------------------------------

%% NOTE NON-STANDARD ENHANCEMENT:
%%  * Unlike in Django, including a variable that resolves to a compiled 
%%    template is permitted. In Django, it's expected that the variable 
%%    will only resolve to a file name/path. 

tag_include(PS, #tag{extra=S}) ->
    Param = 
        case compile_variable(PS, S) of
            #variable{val=#string{val=FilePath}, filters=[]} ->
                load_included_template(PS, FilePath);
            Var ->
                Var
        end,
    {{?MODULE, render_include, Param}, PS}.

load_included_template(Conf, FilePath) ->
    case etcher_loader:get_template(Conf, FilePath) of
        #etcher_template{} = Tpl ->
            Tpl;
        undefined ->
            ""
    end.

render_include(RS, #etcher_template{} = Tpl) ->
    etcher_renderer:render_template(RS, Tpl);
render_include(RS, #variable{} = Var) ->
    case resolve_variable(RS, Var) of
        #etcher_template{} = Tpl ->
            render_include(RS, Tpl);
        FilePath when ?IS_STRING(FilePath) ->
            T = load_included_template(RS, FilePath),
            render_include(RS, T);
        _ ->
            ""
    end;
render_include(_RS, "") ->
    "".

%%------------------------------------------------------------------------
%% Tag: now
%%------------------------------------------------------------------------

% Note: Django's implementation of this tag is non-intuitive when it comes
%       to parsing out the date format string in the following ways: 
%          (1) The format string must be enclosed in double quotes - single 
%              quotes won't do. 
%          (2) The format string cannot be kept in a variable.
%          (3) Django appears to allow any text surronding a format string, so
%              long as it's not a second double quoted string. So, it won't 
%              complaing if you do: {% now 'yeeHaa' dtfmt "jS F Y H:i" wow %}
%       We will honour restrictions (1) and (2), but will throw a suitable 
%       exception if we see a single quoted string. We won't honour the third 
%       restriction - or lack thereof.
tag_now(PS, #tag{extra=S}) ->
    DateFormatString = parse_date_format_string(S),
    {{?MODULE, render_now, DateFormatString}, PS}.

parse_date_format_string([C | _] = S) when (C =:= $') or (C =:= $") ->
    case etcher_scanner:scan_string(S) of
        {ok, #string{val=DateFmtStr}, []} when C =:= $" ->
            DateFmtStr;
        {ok, #string{}, []} when C =:= $' ->
            ErrStr = "For compatibility with Django, the date format string "
                    "in a 'now' tag must be a double-quoted string - not "
                    "a single quoted string.",
            invalid_syntax(ErrStr);
        _ ->
            invalid_date_format_string()
    end;
parse_date_format_string(_) ->
    invalid_date_format_string().

invalid_date_format_string() ->
    ErrStr = "'now' tag requires one argument - a date format string "
                "which must be a double-quoted string",
    invalid_syntax(ErrStr).

render_now(_RS, DateFormatString) ->
    etcher_dateformat:format_date(DateFormatString).

%%------------------------------------------------------------------------
%% Tag: regroup
%%------------------------------------------------------------------------

tag_regroup(PS, #tag{extra=S}) ->
    RegroupArgs = parse_regroup_statement(S, PS),
    {{?MODULE, render_regroup, RegroupArgs}, PS}.

parse_regroup_statement(S, PS) ->
    case string_split(S) of
        [VarStr, "by", Grouper, "as", TargetVarName] ->
            SourceVar = compile_variable(PS, VarStr),
            {SourceVar, Grouper, TargetVarName};
        L when length(L) =/= 5 ->
            invalid_syntax("'regroup' tag takes five arguments");
        [_, _Wrong, _, "as", _] ->
            invalid_syntax("second argument to 'regroup' tag must be 'by'");
        [_, "by", _, _Wrong, _] ->
            ErrStr = "next-to-last argument to 'regroup' tag must be 'as'",
            invalid_syntax(ErrStr)
    end.

render_regroup(RS, {SourceVar, Grouper, TargetVarName}) ->
    case resolve_variable(RS, SourceVar) of
        L when length(L) > 0 ->
            case regroup(L, Grouper) of
                {ok, PList} ->
                    RS1 = update_context(TargetVarName, PList, RS),
                    {RS1, ""};
                failed ->
                    ""
            end;
        _ ->
            ""
    end.
            
regroup(L, Grouper) ->
    regroup(L, Grouper, undefined, [], []).

regroup([PL | Rest], Grouper, LastVal, AccL, AccG) when is_list(PL) ->
    case proplists:lookup(Grouper, PL) of
        {_, LastVal} ->
            regroup(Rest, Grouper, LastVal, [PL | AccL], AccG);
        {_, NewVal} when AccL =:= [] ->                 % first item
            regroup(Rest, Grouper, NewVal, [PL], AccG);
        {_, NewVal} ->       % first item
            GroupObj = new_group_obj(LastVal, lists:reverse(AccL)),
            AccG1 = [GroupObj | AccG],
            regroup(Rest, Grouper, NewVal, [PL], AccG1);
        none ->
            failed
    end;
regroup([_ | _], _Grouper, _LastVal, _AccL, _AccG) ->
    failed;
regroup([], _Grouper, LastVal, AccL, AccG) ->
    GroupObj = new_group_obj(LastVal, lists:reverse(AccL)),
    AccG1 = [GroupObj | AccG],
    L = lists:reverse(AccG1),
    {ok, L}.

new_group_obj(Grouper, List) ->
    [{"grouper", Grouper},
     {"list", List}].

%%------------------------------------------------------------------------
%% Tag: spaceless
%%------------------------------------------------------------------------

tag_spaceless(PS, _Tag) ->
    {Parsed, PS1} = parse_until(PS, "endspaceless"),
    {{?MODULE, render_spaceless, Parsed}, PS1}.

% COPYRIGHT: Implementation copied from: 
%            django.utils.html.strip_spaces_between_tags()
render_spaceless(RS, Underlings) ->
    {RS1, Output} = render(RS, Underlings),
    L = re:replace(Output, ">\\s+<", "><", [unicode, global, {return, iodata}]),
    {RS1, L}.

%%------------------------------------------------------------------------
%% Tag: ssi
%%------------------------------------------------------------------------

tag_ssi(PS, #tag{extra=S}) ->
    {FilePath, DoParse} =
        case string_split(S) of
            ["/" ++ _ = Path] ->
                {Path, false};
            ["/" ++ _ = Path, "parsed"] ->
                {Path, true};
            _ ->
                ErrStr = "'ssi' tag syntax is: "
                                "{% ssi /absolute/path [parsed] %}",
                invalid_syntax(ErrStr)
        end,
    FilePath1 = etcher_util:normalize_absolute_path(FilePath),
    {{?MODULE, render_ssi, {FilePath1, DoParse}}, PS}.

render_ssi(RS, {FilePath, DoParse}) ->
    case include_is_allowed(RS, FilePath) of
        true ->
            ssi_file(FilePath, DoParse, RS);
        false ->
            ""
    end.

include_is_allowed(#rs{allowed_include_roots=AllowedPrefixes}, FilePath) ->
    HasPrefix = fun(P) -> lists:prefix(P, FilePath) end,
    lists:any(HasPrefix, AllowedPrefixes).

ssi_file(FilePath, DoParse, RS) ->
    case file:read_file(FilePath) of
        {ok, Content} when DoParse ->
            compile_and_render(Content, RS);
        {ok, Content} ->
            Content;
        _ ->
            ""
    end.

compile_and_render(Content, #rs{compiler_opts=CompilerOpts} = RS) ->
    try 
        {ok, Template} = etcher_compiler:compile(Content, CompilerOpts),
        etcher_renderer:render_template(RS, Template)
    catch
        throw:_ ->
            ""
    end.

%%------------------------------------------------------------------------
%% Tag: templatetag
%%------------------------------------------------------------------------

tag_templatetag(PS, #tag{extra=SymName}) ->
    S = case SymName of
            "openblock" ->
                "{%";
            "closeblock" ->
                "%}";
            "openvariable" ->
                "{{";
            "closevariable" ->
                "}}";
            "openbrace" ->
                "{";
            "closebrace" ->
                "}";
            "opencomment" ->
                "{#";
            "closecomment" ->
                "#}";
            Unknown ->
                ErrStr = "Invalid templatetag argument: '" ++ Unknown ++ "'.",
                invalid_syntax(ErrStr)
        end,
    {{?MODULE, render_templatetag, S}, PS}.

render_templatetag(_RS, SymStr) ->
    SymStr.

%%------------------------------------------------------------------------
%% Tag: url
%%------------------------------------------------------------------------

tag_url(PS, #tag{extra=S}) ->
    UrlSpec = parse_urlspec(S),
    {{?MODULE, render_url, UrlSpec}, PS}.

parse_urlspec(S) ->
    Regex = "^([^\\s]+)(.*?)(?:\\s+as\\s*([^,\\s]*))?$",
    case string_match(S, Regex) of
        {match, [Req, Extra]} ->
            parse_urlspec(Req, Extra, "");
        {match, [Req, Extra, SaveAs]} ->
            parse_urlspec(Req, Extra, SaveAs);
        nomatch ->
            invalid_syntax("'url' tag takes at least one argument")
    end.

parse_urlspec(Req, Extra, SaveAs) ->
    UrlParams = 
        case trim(Extra) of
            "" ->
                [Req];
            S ->
                Extra1 = string_split(S, "\\s*,\\s*", list),
                [Req | Extra1]
        end,
    SaveAs1 = trim(SaveAs),
    {UrlParams, SaveAs1}.

render_url(#rs{url_mapper=undefined}, _UrlSpec) ->
    "";
render_url(#rs{url_mapper=UrlMapper} = RS, {UrlParams, SaveAs}) ->
    Result = UrlMapper(UrlParams),
    try unicode:characters_to_list(Result) of
        Url when SaveAs =:= "" ->
            Url;
        Url ->
            RS1 = update_context(SaveAs, Url, RS),
            {RS1, ""}
    catch
        error:badarg ->
            ""
    end.

%%------------------------------------------------------------------------
%% Tag: widthratio
%%------------------------------------------------------------------------

tag_widthratio(PS, #tag{extra=S}) ->
    case string_split(S) of
        [_, _, _] = L ->
            Vars = to_variables(PS, L),
            {{?MODULE, render_widthratio, Vars}, PS};
        _ ->
            invalid_syntax("'widthratio' takes three arguments")
    end.

% COPYRIGHT: Implementation translated from Django version in class
%            django.template.WidthRatioNode
render_widthratio(RS, Vars) ->
    [V1, V2, V3] = [resolve_variable(RS, V) || V <- Vars],
    try
        Value = to_float(V1),
        MaxValue = to_float(V2),
        MaxWidth = to_integer(V3),
        Ratio = (Value / MaxValue) * MaxWidth,
        integer_to_list(round(Ratio))
    catch 
        error:badarg ->
            "";
        error:badarith ->
            ""
    end.

%%------------------------------------------------------------------------
%% Tag: with
%%------------------------------------------------------------------------

tag_with(PS, #tag{extra=S}) ->
    {Name, Var} = parse_with_statement(S, PS),
    {Parsed, PS1} = parse_until(PS, "endwith"),
    {{?MODULE, render_with, {Parsed, Name, Var}}, PS1}.

parse_with_statement(Statement, PS) ->
    case string_split(Statement) of
        [VarStr, "as", Name] ->
            Var = compile_variable(PS, VarStr),
            {Name, Var};
        _ ->
            ErrStr = "'with' expected format is 'value as name'",
            invalid_syntax(ErrStr)
    end.

render_with(RS, {Underlings, Name, Var}) ->
    Value = resolve_variable(RS, Var),
    RS1 = update_context(Name, Value, RS), 
    render(RS1, Underlings).

%%------------------------------------------------------------------------
%% Misc.
%%------------------------------------------------------------------------

% NOTE: different return types depending on parameters
parse_until(PS, EndTagName) when ?IS_STRING(EndTagName) ->
    {EndTagName, Parsed, PS1} = parse_until(PS, [EndTagName]),
    {Parsed, PS1};
parse_until(PS, [S | _] = EndTagNames) when ?IS_STRING(S) ->
    case etcher_parser:parse_until(PS, EndTagNames) of
        {#tag{name=TagName, extra=""}, Parsed, PS1} ->
            {TagName, Parsed, PS1};
        {#tag{} = Tag, _Parsed, _PS1} ->
             throw({end_tag_has_extra_content, Tag})
    end.

render(RS, L) ->
    etcher_renderer:render_parts(RS, L).

compile_variable(PS, VarStr) ->
    etcher_parser:compile_variable(PS, VarStr).

resolve_variable(RS, Var) ->
    etcher_renderer:resolve_variable(RS, Var).

render_variable(RS, Var) ->
    etcher_renderer:render_variable(RS, Var).

update_context([{Name, Val} | Rest], RS) ->
    RS1 = update_context(Name, Val, RS),
    update_context(Rest, RS1);
update_context([], RS) ->
    RS.

update_context(Name, Value, #rs{context=Context} = RS) ->
    Context1 = [{Name, Value} | proplists:delete(Name, Context)],
    RS#rs{context=Context1}.

update_globals(Name, Value, #rs{globals=Globals} = RS) ->
    Globals1 = [{Name, Value} | proplists:delete(Name, Globals)],
    RS#rs{globals=Globals1}.

invalid_syntax(ErrStr) ->
    throw({invalid_template_syntax, ErrStr}).

stringify(T) ->
    String = etcher_util:to_string(T),
    CharData = etcher_util:to_iolist(String),
    unicode:characters_to_list(CharData).

string_split(S) ->
    string_split(S, "\\s+", list).

string_split(S, Regex, RetType) ->
    etcher_util:split(S, Regex, [unicode, dotall, {return, RetType}]).

string_match(S, Regex) ->
    string_match(S, Regex, list).

string_match(S, Regex, RetType) ->
    re:run(S, Regex, [unicode, dotall, {capture, all_but_first, RetType}]).

is_var_true(Var, RS) ->
    case resolve_variable(RS, Var) of
        Value when ?EVALS_TO_FALSE(Value) ->
            false;
        _ ->
            true
    end.

to_integer(Val) ->
    etcher_util:to_integer(Val).

to_float(Val) ->
    etcher_util:to_float(Val).

trim(S) ->
    etcher_util:trim(S).

expand_record(Rec) when is_record(Rec, forloop) ->
    expand_record(Rec, record_info(fields, forloop)).

expand_record(Rec, Fields) ->
    [_ | Values] = tuple_to_list(Rec),
    F = fun(Name, Value) -> {atom_to_list(Name), Value} end,
    lists:zipwith(F, Fields, Values).

unique_key() ->
    Hash = etcher_util:hash(now()),
    "_key_" ++ integer_to_list(Hash).

to_variables(PS, VarStrs) ->
    [compile_variable(PS, S) || S <- VarStrs].

