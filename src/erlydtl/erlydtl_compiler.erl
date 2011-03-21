%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% ErlyDTL template compiler
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-12-16 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_compiler).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-export([compile/2, compile/3, compile_dir/2, compile_dir/3, parse/1]).

-record(dtl_context, {
    local_scopes = [], 
    block_dict = dict:new(), 
    auto_escape = off, 
    doc_root = "", 
    parse_trail = [],
    vars = [],
    custom_tags_dir = [],
    custom_tags_module = none,
    reader = {file, read_file},
    module = [],
    compiler_options = [verbose, report_errors],
    force_recompile = false,
    locale = none}).

-record(ast_info, {
    dependencies = [],
    translatable_strings = [],
    custom_tags = [],
    var_names = [],
    pre_render_asts = []}).
    
-record(treewalker, {
    counter = 0,
    safe = false
}).    

compile(Binary, Module) when is_binary(Binary) ->
    compile(Binary, Module, []);

compile(File, Module) ->
    compile(File, Module, []).

compile(Binary, Module, Options) when is_binary(Binary) ->
    File = "",
    CheckSum = "",
    case parse(Binary) of
        {ok, DjangoParseTree} ->
            case compile_to_binary(File, DjangoParseTree, 
                    init_dtl_context(File, Module, Options), CheckSum) of
                {ok, Module1, _} ->
                    {ok, Module1};
                Err ->
                    Err
            end;
        Err ->
            Err
    end;
    
compile(File, Module, Options) ->  
    Context = init_dtl_context(File, Module, Options),
    case parse(File, Context) of  
        ok ->
            ok;
        {ok, DjangoParseTree, CheckSum} ->
            case compile_to_binary(File, DjangoParseTree, Context, CheckSum) of
                {ok, Module1, Bin} ->
                    write_binary(Module1, Bin, Options);
                Err ->
                    Err
            end;
        Err ->
            Err
    end.
    

compile_dir(Dir, Module) ->
    compile_dir(Dir, Module, []).

compile_dir(Dir, Module, Options) ->
    Context = init_dtl_context_dir(Dir, Module, Options),
    Files = case file:list_dir(Dir) of
        {ok, FileList} -> FileList;
        _ -> []
    end,
    {ParserResults, ParserErrors} = lists:foldl(fun
            ("."++_, Acc) -> Acc;
            (File, {ResultAcc, ErrorAcc}) ->
                FilePath = filename:join([Dir, File]),
                case parse(FilePath, Context) of
                    ok -> {ResultAcc, ErrorAcc};
                    {ok, DjangoParseTree, CheckSum} -> {[{File, DjangoParseTree, CheckSum}|ResultAcc], ErrorAcc};
                    Err -> {ResultAcc, [Err|ErrorAcc]}
                end
        end, {[], []}, Files),
    case ParserErrors of
        [] ->
            case compile_multiple_to_binary(Dir, ParserResults, Context) of
                {ok, Module1, Bin} ->
                    write_binary(Module1, Bin, Options);
                Err ->
                    Err
            end;
        [Error|_] ->
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

write_binary(Module1, Bin, Options) ->
    case proplists:get_value(out_dir, Options) of
        undefined ->
            ok;
        OutDir ->
            BeamFile = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
            case file:write_file(BeamFile, Bin) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, lists:concat(["beam generation failed (", Reason, "): ", BeamFile])}
            end
    end.

compile_multiple_to_binary(Dir, ParserResults, Context) ->
    {Functions, {AstInfo, _}} = lists:mapfoldl(fun({File, DjangoParseTree, CheckSum}, {AstInfo, TreeWalker}) ->
                FilePath = full_path(File, Context#dtl_context.doc_root),
                {{BodyAst, BodyInfo}, TreeWalker1} = with_dependency({FilePath, CheckSum}, body_ast(DjangoParseTree, Context, TreeWalker)),
                FunctionName = filename:rootname(filename:basename(File)),
                Function1 = erl_syntax:function(erl_syntax:atom(FunctionName),
                    [erl_syntax:clause([erl_syntax:variable("Variables")], none,
                            [erl_syntax:application(none, erl_syntax:atom(FunctionName), 
                                    [erl_syntax:variable("Variables"), erl_syntax:atom(none)])])]),
                Function2 = erl_syntax:function(erl_syntax:atom(FunctionName), 
                    [erl_syntax:clause([erl_syntax:variable("Variables"), erl_syntax:variable("TranslationFun")], none,
                            [BodyAst])]),
                {{FunctionName, Function1, Function2}, {merge_info(AstInfo, BodyInfo), TreeWalker1}}
        end, {#ast_info{}, #treewalker{}}, ParserResults),
    Forms = custom_forms(Dir, Context#dtl_context.module, Functions, AstInfo),
    compile_forms_and_reload(Dir, Forms, Context#dtl_context.compiler_options).

compile_to_binary(File, DjangoParseTree, Context, CheckSum) ->
    try body_ast(DjangoParseTree, Context, #treewalker{}) of
        {{BodyAst, BodyInfo}, BodyTreeWalker} ->
            try custom_tags_ast(BodyInfo#ast_info.custom_tags, Context, BodyTreeWalker) of
                {{CustomTagsAst, CustomTagsInfo}, _} ->
                    Forms = forms(File, Context#dtl_context.module, {BodyAst, BodyInfo}, {CustomTagsAst, CustomTagsInfo}, CheckSum), 
                    compile_forms_and_reload(File, Forms, Context#dtl_context.compiler_options)
            catch 
                throw:Error -> Error
            end
    catch 
        throw:Error -> Error
    end.

compile_forms_and_reload(File, Forms, CompilerOptions) ->
    case compile:forms(Forms, CompilerOptions) of
        {ok, Module1, Bin} -> 
            code:purge(Module1),
            case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                {module, _} -> {ok, Module1, Bin};
                _ -> {error, lists:concat(["code reload failed: ", Module1])}
            end;
        error ->
            {error, lists:concat(["compilation failed: ", File])};
        OtherError ->
            OtherError
    end.
                
init_dtl_context(File, Module, Options) when is_list(Module) ->
    init_dtl_context(File, list_to_atom(Module), Options);
init_dtl_context(File, Module, Options) ->
    Ctx = #dtl_context{},
    #dtl_context{
        parse_trail = [File], 
        module = Module,
        doc_root = proplists:get_value(doc_root, Options, filename:dirname(File)),
        custom_tags_dir = proplists:get_value(custom_tags_dir, Options, filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags"])),
        custom_tags_module = proplists:get_value(custom_tags_module, Options, Ctx#dtl_context.custom_tags_module),
        vars = proplists:get_value(vars, Options, Ctx#dtl_context.vars), 
        reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
        compiler_options = proplists:get_value(compiler_options, Options, Ctx#dtl_context.compiler_options),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#dtl_context.force_recompile),
        locale = proplists:get_value(locale, Options, Ctx#dtl_context.locale)}.

init_dtl_context_dir(Dir, Module, Options) when is_list(Module) ->
    init_dtl_context_dir(Dir, list_to_atom(Module), Options);
init_dtl_context_dir(Dir, Module, Options) ->
    Ctx = #dtl_context{},
    #dtl_context{
        parse_trail = [], 
        module = Module,
        doc_root = proplists:get_value(doc_root, Options, Dir),
        custom_tags_dir = proplists:get_value(custom_tags_dir, Options, filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags"])),
        custom_tags_module = proplists:get_value(custom_tags_module, Options, Module),
        vars = proplists:get_value(vars, Options, Ctx#dtl_context.vars), 
        reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
        compiler_options = proplists:get_value(compiler_options, Options, Ctx#dtl_context.compiler_options),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#dtl_context.force_recompile),
        locale = proplists:get_value(locale, Options, Ctx#dtl_context.locale)}.


is_up_to_date(_, #dtl_context{force_recompile = true}) ->
    false;
is_up_to_date(CheckSum, Context) ->
    Module = Context#dtl_context.module,
    {M, F} = Context#dtl_context.reader,
    case catch Module:source() of
        {_, CheckSum} -> 
            case catch Module:dependencies() of
                L when is_list(L) ->
                    RecompileList = lists:foldl(fun
                            ({XFile, XCheckSum}, Acc) ->
                                case catch M:F(XFile) of
                                    {ok, Data} ->
                                        case binary_to_list(erlang:md5(Data)) of
                                            XCheckSum ->
                                                Acc;
                                            _ ->
                                                [recompile | Acc]
                                        end;
                                    _ ->
                                        [recompile | Acc]
                                end                                        
                        end, [], L),
                    case RecompileList of
                        [] -> true; 
                        _ -> false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.
    
    
parse(File, Context) ->  
    {M, F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            CheckSum = binary_to_list(erlang:md5(Data)),
            case parse(CheckSum, Data, Context) of
                {error, Msg} when is_list(Msg) ->
                    {error, File ++ ": " ++ Msg};
                {error, Msg} ->
                    {error, {File, [Msg]}};
                Result ->
                    Result
            end;
        _ ->
            {error, {File, [{0, Context#dtl_context.module, "Failed to read file"}]}}
    end.
        
parse(CheckSum, Data, Context) ->
    case is_up_to_date(CheckSum, Context) of
        true ->
            ok;
        _ ->
            case parse(Data) of
                {ok, Val} ->
                    {ok, Val, CheckSum};
                Err ->
                    Err
            end
    end.

parse(Data) ->
    case erlydtl_scanner:scan(binary_to_list(Data)) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.        

custom_tags_ast(CustomTags, Context, TreeWalker) ->
    {{CustomTagsClauses, CustomTagsInfo}, TreeWalker1} = custom_tags_clauses_ast(CustomTags, Context, TreeWalker),
    {{erl_syntax:function(erl_syntax:atom(render_tag), CustomTagsClauses), CustomTagsInfo}, TreeWalker1}.

custom_tags_clauses_ast(CustomTags, Context, TreeWalker) ->
    custom_tags_clauses_ast1(CustomTags, [], [], #ast_info{}, Context, TreeWalker).

custom_tags_clauses_ast1([], _ExcludeTags, ClauseAcc, InfoAcc, _Context, TreeWalker) ->
    {{lists:reverse([erl_syntax:clause([erl_syntax:underscore(), erl_syntax:underscore(), erl_syntax:underscore()], none,
                        [erl_syntax:list([])])|ClauseAcc]), InfoAcc}, TreeWalker};
custom_tags_clauses_ast1([Tag|CustomTags], ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker) ->
    case lists:member(Tag, ExcludeTags) of
        true ->
            custom_tags_clauses_ast1(CustomTags, ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker);
        false ->
            CustomTagFile = full_path(Tag, Context#dtl_context.custom_tags_dir),
            case parse(CustomTagFile, Context) of
                {ok, DjangoParseTree, CheckSum} ->
                    {{BodyAst, BodyAstInfo}, TreeWalker1} = with_dependency({CustomTagFile, CheckSum}, 
                        body_ast(DjangoParseTree, Context, TreeWalker)),
                    Clause = erl_syntax:clause([erl_syntax:string(Tag), erl_syntax:variable("Variables"), erl_syntax:variable("TranslationFun")],
                                none, [BodyAst]),
                    custom_tags_clauses_ast1(CustomTags, [Tag|ExcludeTags], [Clause|ClauseAcc], merge_info(BodyAstInfo, InfoAcc), 
                        Context, TreeWalker1);
                Error ->
                    throw(Error)
            end
    end.

dependencies_function(Dependencies) ->
    erl_syntax:function(
        erl_syntax:atom(dependencies), [erl_syntax:clause([], none, 
            [erl_syntax:list(lists:map(fun 
                    ({XFile, XCheckSum}) -> 
                        erl_syntax:tuple([erl_syntax:string(XFile), erl_syntax:string(XCheckSum)])
                end, Dependencies))])]).

translatable_strings_function(TranslatableStrings) ->
        erl_syntax:function(
        erl_syntax:atom(translatable_strings), [erl_syntax:clause([], none,
                [erl_syntax:list(lists:map(fun(String) -> erl_syntax:string(String) end,
                            TranslatableStrings))])]).

custom_forms(Dir, Module, Functions, AstInfo) ->
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(source_dir), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0))
                    | 
                        lists:foldl(fun({FunctionName, _, _}, Acc) ->
                            [erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(1)),
                                erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(2))|Acc]
                    end, [], Functions)]
            )]),
    SourceFunctionAst = erl_syntax:function(
        erl_syntax:atom(source_dir), [erl_syntax:clause([], none, [erl_syntax:string(Dir)])]),
    DependenciesFunctionAst = dependencies_function(AstInfo#ast_info.dependencies), 
    TranslatableStringsFunctionAst = translatable_strings_function(AstInfo#ast_info.translatable_strings),
    FunctionAsts = lists:foldl(fun({_, Function1, Function2}, Acc) -> [Function1, Function2 | Acc] end, [], Functions),

    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsFunctionAst
            | FunctionAsts]].

forms(File, Module, {BodyAst, BodyInfo}, {CustomTagsFunctionAst, CustomTagsInfo}, CheckSum) ->
    MergedInfo = merge_info(BodyInfo, CustomTagsInfo),
    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([], none, [erl_syntax:application(none, 
                        erl_syntax:atom(render), [erl_syntax:list([])])])]),
    Render1FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables")], none, 
                [erl_syntax:application(none,
                        erl_syntax:atom(render),
                        [erl_syntax:variable("Variables"), erl_syntax:atom(none)])])]),
    Function2 = erl_syntax:application(none, erl_syntax:atom(render_internal), 
        [erl_syntax:variable("Variables"), erl_syntax:variable("TranslationFun")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
        [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
        [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
    Render2FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables"), 
                    erl_syntax:variable("TranslationFun")], none, 
            [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  
     
    SourceFunctionTuple = erl_syntax:tuple(
        [erl_syntax:string(File), erl_syntax:string(CheckSum)]),
    SourceFunctionAst = erl_syntax:function(
        erl_syntax:atom(source),
            [erl_syntax:clause([], none, [SourceFunctionTuple])]),
    
    DependenciesFunctionAst = dependencies_function(MergedInfo#ast_info.dependencies),

    TranslatableStringsAst = translatable_strings_function(MergedInfo#ast_info.translatable_strings),

    BodyAstTmp = erl_syntax:application(
                    erl_syntax:atom(erlydtl_runtime),
                    erl_syntax:atom(stringify_final),
                    [BodyAst]),

    RenderInternalFunctionAst = erl_syntax:function(
        erl_syntax:atom(render_internal), 
        [erl_syntax:clause([erl_syntax:variable("Variables"), erl_syntax:variable("TranslationFun")], none, 
                [BodyAstTmp])]),   
    
    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(1)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(2)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0))])]),
    
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, Render0FunctionAst, Render1FunctionAst, Render2FunctionAst,
            SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsAst, RenderInternalFunctionAst, CustomTagsFunctionAst
            | BodyInfo#ast_info.pre_render_asts]].    

        
% child templates should only consist of blocks at the top level
body_ast([{extends, {string_literal, _Pos, String}} | ThisParseTree], Context, TreeWalker) ->
    File = full_path(unescape_string_literal(String), Context#dtl_context.doc_root),
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            throw({error, "Circular file inclusion!"});
        _ ->
            case parse(File, Context) of
                {ok, ParentParseTree, CheckSum} ->
                    BlockDict = lists:foldl(
                        fun
                            ({block, {identifier, _, Name}, Contents}, Dict) ->
                                dict:store(Name, Contents, Dict);
                            (_, Dict) ->
                                Dict
                        end, dict:new(), ThisParseTree),
                    with_dependency({File, CheckSum}, body_ast(ParentParseTree, Context#dtl_context{
                        block_dict = dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                            BlockDict, Context#dtl_context.block_dict),
                                parse_trail = [File | Context#dtl_context.parse_trail]}, TreeWalker));
                Err ->
                    throw(Err)
            end        
    end;
 
    
body_ast(DjangoParseTree, Context, TreeWalker) ->
    {AstInfoList, TreeWalker2} = lists:mapfoldl(
        fun
            ({'autoescape', {identifier, _, OnOrOff}, Contents}, TreeWalkerAcc) ->
                body_ast(Contents, Context#dtl_context{auto_escape = OnOrOff}, 
                    TreeWalkerAcc);
            ({'block', {identifier, _, Name}, Contents}, TreeWalkerAcc) ->
                Block = case dict:find(Name, Context#dtl_context.block_dict) of
                    {ok, ChildBlock} -> ChildBlock;
                    _ -> Contents
                end,
                body_ast(Block, Context, TreeWalkerAcc);
            ({'call', {'identifier', _, Name}}, TreeWalkerAcc) ->
            	call_ast(Name, TreeWalkerAcc);
            ({'call', {'identifier', _, Name}, With}, TreeWalkerAcc) ->
            	call_with_ast(Name, With, Context, TreeWalkerAcc);
            ({'comment', _Contents}, TreeWalkerAcc) ->
                empty_ast(TreeWalkerAcc);
            ({'cycle', Names}, TreeWalkerAcc) ->
                cycle_ast(Names, Context, TreeWalkerAcc);
            ({'cycle_compat', Names}, TreeWalkerAcc) ->
                cycle_compat_ast(Names, Context, TreeWalkerAcc);
            ({'date', 'now', {string_literal, _Pos, FormatString}}, TreeWalkerAcc) ->
                now_ast(FormatString, Context, TreeWalkerAcc);
            ({'filter', FilterList, Contents}, TreeWalkerAcc) ->
                filter_tag_ast(FilterList, Contents, Context, TreeWalkerAcc);
            ({'firstof', Vars}, TreeWalkerAcc) ->
                firstof_ast(Vars, Context, TreeWalkerAcc);
            ({'for', {'in', IteratorList, Variable}, Contents}, TreeWalkerAcc) ->
                {EmptyAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                for_loop_ast(IteratorList, Variable, Contents, EmptyAstInfo, Context, TreeWalker1);
            ({'for', {'in', IteratorList, Variable}, Contents, EmptyPartContents}, TreeWalkerAcc) ->
                {EmptyAstInfo, TreeWalker1} = body_ast(EmptyPartContents, Context, TreeWalkerAcc),
                for_loop_ast(IteratorList, Variable, Contents, EmptyAstInfo, Context, TreeWalker1);
            ({'if', Expression, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifelse_ast(Expression, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifelse', Expression, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                ifelse_ast(Expression, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifequal', [Arg1, Arg2], Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifelse_ast({'expr', "eq", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifequalelse', [Arg1, Arg2], IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc), 
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context,TreeWalker1),
                ifelse_ast({'expr', "eq", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                
            ({'ifnotequal', [Arg1, Arg2], Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifelse_ast({'expr', "ne", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifnotequalelse', [Arg1, Arg2], IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                ifelse_ast({'expr', "ne", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                    
            ({'include', {string_literal, _, File}, Args}, TreeWalkerAcc) ->
                include_ast(unescape_string_literal(File), Args, Context#dtl_context.local_scopes, Context, TreeWalkerAcc);
            ({'include_only', {string_literal, _, File}, Args}, TreeWalkerAcc) ->
                include_ast(unescape_string_literal(File), Args, [], Context, TreeWalkerAcc);
            ({'spaceless', Contents}, TreeWalkerAcc) ->
                spaceless_ast(Contents, Context, TreeWalkerAcc);
            ({'ssi', Arg}, TreeWalkerAcc) ->
                ssi_ast(Arg, Context, TreeWalkerAcc);
            ({'ssi_parsed', {string_literal, _, FileName}}, TreeWalkerAcc) ->
                include_ast(unescape_string_literal(FileName), [], Context#dtl_context.local_scopes, Context, TreeWalkerAcc);
            ({'string', _Pos, String}, TreeWalkerAcc) -> 
                string_ast(String, TreeWalkerAcc);
            ({'tag', {'identifier', _, Name}, Args}, TreeWalkerAcc) ->
                tag_ast(Name, Args, Context, TreeWalkerAcc);            
            ({'templatetag', {_, _, TagName}}, TreeWalkerAcc) ->
                templatetag_ast(TagName, Context, TreeWalkerAcc);
	    ({'trans', Value}, TreeWalkerAcc) ->
                translated_ast(Value, Context, TreeWalkerAcc);
            ({'widthratio', Numerator, Denominator, Scale}, TreeWalkerAcc) ->
                widthratio_ast(Numerator, Denominator, Scale, Context, TreeWalkerAcc);
            ({'with', Args, Contents}, TreeWalkerAcc) ->
                with_ast(Args, Contents, Context, TreeWalkerAcc);
            (ValueToken, TreeWalkerAcc) -> 
                {{ValueAst,ValueInfo},ValueTreeWalker} = value_ast(ValueToken, true, Context, TreeWalkerAcc),
                {{format(ValueAst, Context, ValueTreeWalker),ValueInfo},ValueTreeWalker}
        end, TreeWalker, DjangoParseTree),   
    {AstList, {Info, TreeWalker3}} = lists:mapfoldl(
        fun({Ast, Info}, {InfoAcc, TreeWalkerAcc}) -> 
                PresetVars = lists:foldl(fun
                        (X, Acc) ->
                            case proplists:lookup(X, Context#dtl_context.vars) of
                                none ->
                                    Acc;
                                Val ->
                                    [erl_syntax:abstract(Val) | Acc]
                            end
                    end, [], Info#ast_info.var_names),
                case PresetVars of
                    [] ->
                        {Ast, {merge_info(Info, InfoAcc), TreeWalkerAcc}};
                    _ ->
                        Counter = TreeWalkerAcc#treewalker.counter,
                        Name = lists:concat([pre_render, Counter]),
                        Ast1 = erl_syntax:application(none, erl_syntax:atom(Name),
                            [erl_syntax:list(PresetVars)]),
                        PreRenderAst = erl_syntax:function(erl_syntax:atom(Name),
                            [erl_syntax:clause([erl_syntax:variable("Variables")], none, [Ast])]),
                        PreRenderAsts = Info#ast_info.pre_render_asts,
                        Info1 = Info#ast_info{pre_render_asts = [PreRenderAst | PreRenderAsts]},     
                        {Ast1, {merge_info(Info1, InfoAcc), TreeWalkerAcc#treewalker{counter = Counter + 1}}}
                end
        end, {#ast_info{}, TreeWalker2}, AstInfoList),
    {{erl_syntax:list(AstList), Info}, TreeWalker3}.


value_ast(ValueToken, AsString, Context, TreeWalker) ->
    case ValueToken of
        {'expr', Operator, Value} ->
            {{ValueAst,InfoValue}, TreeWalker1} = value_ast(Value, false, Context, TreeWalker),
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
                                         erl_syntax:atom(Operator), 
                                         [ValueAst]),
            {{Ast, InfoValue}, TreeWalker1};
        {'expr', Operator, Value1, Value2} ->
            {{Value1Ast,InfoValue1}, TreeWalker1} = value_ast(Value1, false, Context, TreeWalker),
            {{Value2Ast,InfoValue2}, TreeWalker2} = value_ast(Value2, false, Context, TreeWalker1),
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
                                         erl_syntax:atom(Operator), 
                                         [Value1Ast, Value2Ast]),
            {{Ast, merge_info(InfoValue1,InfoValue2)}, TreeWalker2};
        {'string_literal', _Pos, String} ->
            {{erl_syntax:string(unescape_string_literal(String)), #ast_info{}}, TreeWalker};
        {'number_literal', _Pos, Number} ->
            case AsString of
                true  -> string_ast(Number, TreeWalker);
                false -> {{erl_syntax:integer(list_to_integer(Number)), #ast_info{}}, TreeWalker}
            end;
        {'apply_filter', Variable, Filter} ->
            filter_ast(Variable, Filter, Context, TreeWalker);
        {'attribute', _} = Variable ->
            {Ast, VarName} = resolve_variable_ast(Variable, Context),
            {{Ast, #ast_info{var_names = [VarName]}}, TreeWalker};
        {'variable', _} = Variable ->
            {Ast, VarName} = resolve_variable_ast(Variable, Context),
            {{Ast, #ast_info{var_names = [VarName]}}, TreeWalker}
    end.

merge_info(Info1, Info2) ->
    #ast_info{
        dependencies = 
            lists:merge(
                lists:sort(Info1#ast_info.dependencies), 
                lists:sort(Info2#ast_info.dependencies)),
        var_names = 
            lists:merge(
                lists:sort(Info1#ast_info.var_names), 
                lists:sort(Info2#ast_info.var_names)),
        translatable_strings =
            lists:merge(
                lists:sort(Info1#ast_info.translatable_strings),
                lists:sort(Info2#ast_info.translatable_strings)),
        custom_tags = 
            lists:merge(
                lists:sort(Info1#ast_info.custom_tags),
                lists:sort(Info2#ast_info.custom_tags)),
        pre_render_asts = 
            lists:merge(
                Info1#ast_info.pre_render_asts,
                Info2#ast_info.pre_render_asts)}.


with_dependencies([], Args) ->
    Args;
with_dependencies([Dependency | Rest], Args) ->
     with_dependencies(Rest, with_dependency(Dependency, Args)).

with_dependency(FilePath, {{Ast, Info}, TreeWalker}) ->
    {{Ast, Info#ast_info{dependencies = [FilePath | Info#ast_info.dependencies]}}, TreeWalker}.


empty_ast(TreeWalker) ->
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker}.


translated_ast({string_literal, _, String}, Context, TreeWalker) ->
    NewStr = unescape_string_literal(String),
    DefaultString = case Context#dtl_context.locale of
        none -> NewStr;
        Locale -> erlydtl_i18n:translate(NewStr,Locale)
    end,
    translated_ast2(erl_syntax:string(NewStr), erl_syntax:string(DefaultString),
        #ast_info{translatable_strings = [NewStr]}, TreeWalker);
translated_ast(ValueToken, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = value_ast(ValueToken, true, Context, TreeWalker),
    translated_ast2(Ast, Ast, Info, TreeWalker1).

translated_ast2(NewStrAst, DefaultStringAst, AstInfo, TreeWalker) ->
    StringLookupAst = erl_syntax:application(
        erl_syntax:atom(erlydtl_runtime),
        erl_syntax:atom(translate),
        [NewStrAst, erl_syntax:variable("TranslationFun"), DefaultStringAst]),
    {{StringLookupAst, AstInfo}, TreeWalker}.

% Completely unnecessary in ErlyDTL (use {{ "{%" }} etc), but implemented for compatibility.
templatetag_ast('openblock', _Context, TreeWalker) ->
    {{erl_syntax:string("{%"), #ast_info{}}, TreeWalker};
templatetag_ast('closeblock', _Context, TreeWalker) ->
    {{erl_syntax:string("%}"), #ast_info{}}, TreeWalker};
templatetag_ast('openvariable', _Context, TreeWalker) ->
    {{erl_syntax:string("{{"), #ast_info{}}, TreeWalker};
templatetag_ast('closevariable', _Context, TreeWalker) ->
    {{erl_syntax:string("}}"), #ast_info{}}, TreeWalker};
templatetag_ast('openbrace', _Context, TreeWalker) ->
    {{erl_syntax:string("{"), #ast_info{}}, TreeWalker};
templatetag_ast('closebrace', _Context, TreeWalker) ->
    {{erl_syntax:string("}"), #ast_info{}}, TreeWalker};
templatetag_ast('opencomment', _Context, TreeWalker) ->
    {{erl_syntax:string("{#"), #ast_info{}}, TreeWalker};
templatetag_ast('closecomment', _Context, TreeWalker) ->
    {{erl_syntax:string("#}"), #ast_info{}}, TreeWalker}.


widthratio_ast(Numerator, Denominator, Scale, Context, TreeWalker) ->
    {{NumAst, NumInfo}, TreeWalker1} = value_ast(Numerator, false, Context, TreeWalker),
    {{DenAst, DenInfo}, TreeWalker2} = value_ast(Denominator, false, Context, TreeWalker1),
    {{ScaleAst, ScaleInfo}, TreeWalker3} = value_ast(Scale, false, Context, TreeWalker2),
    {{format_number_ast(erl_syntax:application(
                erl_syntax:atom(erlydtl_runtime),
                erl_syntax:atom(widthratio),
                [NumAst, DenAst, ScaleAst])), merge_info(ScaleInfo, merge_info(NumInfo, DenInfo))},
        TreeWalker3}.


string_ast(String, TreeWalker) ->
    {{erl_syntax:string(String), #ast_info{}}, TreeWalker}. %% less verbose AST, better for development and debugging
    % {{erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]), #ast_info{}}, TreeWalker}.       


include_ast(File, ArgList, Scopes, Context, TreeWalker) ->
    FilePath = full_path(File, Context#dtl_context.doc_root),
    case parse(FilePath, Context) of
        {ok, InclusionParseTree, CheckSum} ->
            {NewScope, {ArgInfo, TreeWalker1}} = lists:mapfoldl(fun
                    ({{identifier, _, LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                        {{Ast, Info}, TreeWalker2} = value_ast(Value, false, Context, TreeWalker1),
                        {{LocalVarName, Ast}, {merge_info(AstInfo1, Info), TreeWalker2}}
                end, {#ast_info{}, TreeWalker}, ArgList),

            {{BodyAst, BodyInfo}, TreeWalker2} = with_dependency({FilePath, CheckSum}, 
                body_ast(InclusionParseTree, Context#dtl_context{
                        parse_trail = [FilePath | Context#dtl_context.parse_trail],
                        local_scopes = [NewScope|Scopes]
                    }, TreeWalker1)),

            {{BodyAst, merge_info(BodyInfo, ArgInfo)}, TreeWalker2};
        Err ->
            throw(Err)
    end.
    
% include at run-time
ssi_ast(FileName, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = value_ast(FileName, true, Context, TreeWalker),
    {Mod, Fun} = Context#dtl_context.reader,
    {{erl_syntax:application(
                erl_syntax:atom(erlydtl_runtime),
                erl_syntax:atom(read_file),
                [erl_syntax:atom(Mod), erl_syntax:atom(Fun), erl_syntax:string(Context#dtl_context.doc_root), Ast]), Info}, TreeWalker1}.

filter_tag_ast(FilterList, Contents, Context, TreeWalker) ->
    {{InnerAst, Info}, TreeWalker1} = body_ast(Contents, Context#dtl_context{auto_escape = did}, TreeWalker),
    {{FilteredAst, FilteredInfo}, TreeWalker2} = lists:foldl(fun
            ([{identifier, _, 'escape'}], {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
            ([{identifier, _, 'safe'}], {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
            ([{identifier, _, 'safeseq'}], {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
            (Filter, {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                {Ast, AstInfo} = filter_ast1(Filter, AstAcc, Context),
                {{Ast, merge_info(InfoAcc, AstInfo)}, TreeWalkerAcc}
        end, {{erl_syntax:application(
                    erl_syntax:atom(lists),
                    erl_syntax:atom(flatten),
                    [InnerAst]), Info}, TreeWalker1}, FilterList),

    EscapedAst = case search_for_escape_filter(lists:reverse(FilterList), Context) of
        on ->
            erl_syntax:application(
                erl_syntax:atom(erlydtl_filters), 
                erl_syntax:atom(force_escape), 
                [FilteredAst]);
        _ ->
            FilteredAst
    end,
    {{EscapedAst, FilteredInfo}, TreeWalker2}.

search_for_escape_filter(FilterList, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(FilterList);
search_for_escape_filter(_, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter([[{identifier, _, 'escape'}]|Rest], _Context) ->
    search_for_safe_filter(Rest);
search_for_escape_filter([_|Rest], Context) ->
    search_for_escape_filter(Rest, Context);
search_for_escape_filter([], _Context) ->
    off.

search_for_safe_filter([[{identifier, _, 'safe'}]|_]) ->
    off;
search_for_safe_filter([[{identifier, _, 'safeseq'}]|_]) ->
    off;
search_for_safe_filter([_|Rest]) ->
    search_for_safe_filter(Rest);
search_for_safe_filter([]) ->
    on.

filter_ast(Variable, Filter, Context, TreeWalker) ->
    % the escape filter is special; it is always applied last, so we have to go digging for it

    % AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    % so don't do any more escaping
    {{UnescapedAst, Info}, TreeWalker2} = filter_ast_noescape(Variable, Filter, 
        Context#dtl_context{auto_escape = did}, TreeWalker),
    EscapedAst = case search_for_escape_filter(Variable, Filter, Context) of
        on ->
            erl_syntax:application(
                erl_syntax:atom(erlydtl_filters), 
                erl_syntax:atom(force_escape), 
                [UnescapedAst]);
        _ -> 
            UnescapedAst
    end,
    {{EscapedAst, Info}, TreeWalker2}.

filter_ast_noescape(Variable, [{identifier, _, 'escape'}], Context, TreeWalker) ->
    value_ast(Variable, true, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, [{identifier, _, 'safe'}], Context, TreeWalker) ->
    value_ast(Variable, true, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, [{identifier, _, 'safeseq'}], Context, TreeWalker) ->
    value_ast(Variable, true, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, Filter, Context, TreeWalker) ->
    {{VariableAst, Info1}, TreeWalker2} = value_ast(Variable, true, Context, TreeWalker),
    {VarValue, Info2} = filter_ast1(Filter, VariableAst, Context),
    {{VarValue, merge_info(Info1, Info2)}, TreeWalker2}.

filter_ast1([{identifier, _, Name}, {string_literal, _, ArgName}], VariableAst, _Context) ->
    filter_ast2(Name, VariableAst, [erl_syntax:string(unescape_string_literal(ArgName))], []);
filter_ast1([{identifier, _, Name}, {number_literal, _, ArgName}], VariableAst, _Context) ->
    filter_ast2(Name, VariableAst, [erl_syntax:integer(list_to_integer(ArgName))], []);
filter_ast1([{identifier, _, Name}, ArgVariable], VariableAst, Context) ->
    {ArgAst, ArgVarName} = resolve_variable_ast(ArgVariable, Context),
    filter_ast2(Name, VariableAst, [ArgAst], [ArgVarName]);
filter_ast1([{identifier, _, Name}], VariableAst, _Context) ->
    filter_ast2(Name, VariableAst, [], []).

filter_ast2(Name, VariableAst, AdditionalArgs, VarNames) ->
    {erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(Name), 
            [VariableAst | AdditionalArgs]), #ast_info{var_names = VarNames}}.
 
search_for_escape_filter(Variable, Filter, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(Variable, [{identifier, _, 'escape'}] = Filter, _Context) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter({apply_filter, Variable, Filter}, _, Context) ->
    search_for_escape_filter(Variable, Filter, Context);
search_for_escape_filter(_Variable, _Filter, _Context) ->
    off.

search_for_safe_filter(_, [{identifier, _, 'safe'}]) ->
    off;
search_for_safe_filter(_, [{identifier, _, 'safeseq'}]) ->
    off;
search_for_safe_filter({apply_filter, Variable, Filter}, _) ->
    search_for_safe_filter(Variable, Filter);
search_for_safe_filter(_Variable, _Filter) ->
    on.

resolve_variable_ast(VarTuple, Context) ->
    resolve_variable_ast(VarTuple, Context, 'find_value').
 
resolve_variable_ast({attribute, {{identifier, _, AttrName}, Variable}}, Context, FinderFunction) ->
    {VarAst, VarName} = resolve_variable_ast(Variable, Context, FinderFunction),
    {erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(FinderFunction),
                    [erl_syntax:atom(AttrName), VarAst]), VarName};

resolve_variable_ast({variable, {identifier, _, VarName}}, Context, FinderFunction) ->
    VarValue = case resolve_scoped_variable_ast(VarName, Context) of
        undefined ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(FinderFunction),
                [erl_syntax:atom(VarName), erl_syntax:variable("Variables")]);
        Val ->
            Val
    end,
    {VarValue, VarName};

resolve_variable_ast(What, _Context, _FinderFunction) ->
   error_logger:error_msg("~p:resolve_variable_ast unhandled: ~p~n", [?MODULE, What]).

resolve_scoped_variable_ast(VarName, Context) ->
    lists:foldl(fun(Scope, Value) ->
                case Value of
                    undefined -> proplists:get_value(VarName, Scope);
                    _ -> Value
                end
        end, undefined, Context#dtl_context.local_scopes).

format(Ast, Context, TreeWalker) ->
    auto_escape(format_number_ast(Ast), Context, TreeWalker).

format_number_ast(Ast) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(format_number),
        [Ast]).


auto_escape(Value, _, #treewalker{safe = true}) ->
    Value;
auto_escape(Value, #dtl_context{auto_escape = on}, _) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape), [Value]);
auto_escape(Value, _, _) ->
    Value.

firstof_ast(Vars, Context, TreeWalker) ->
	body_ast([lists:foldr(fun
        ({L, _, _}=Var, []) when L=:=string_literal;L=:=number_literal ->
            Var;
        ({L, _, _}, _) when L=:=string_literal;L=:=number_literal ->
            erlang:error(errbadliteral);
        (Var, []) ->
            {'ifelse', Var, [Var], []};
        (Var, Acc) ->
            {'ifelse', Var, [Var], [Acc]} end,
    	[], Vars)], Context, TreeWalker).

ifelse_ast(Expression, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    {{Ast, ExpressionInfo}, TreeWalker1} = value_ast(Expression, false, Context, TreeWalker), 
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(is_true), [Ast]),
        [erl_syntax:clause([erl_syntax:atom(true)], none, 
                [IfContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none,
                [ElseContentsAst])
        ]), merge_info(ExpressionInfo, Info)}, TreeWalker1}.

with_ast(ArgList, Contents, Context, TreeWalker) ->
    {ArgAstList, {ArgInfo, TreeWalker1}} = lists:mapfoldl(fun
            ({{identifier, _, _LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                {{Ast, Info}, TreeWalker2} = value_ast(Value, false, Context, TreeWalker1),
                {Ast, {merge_info(AstInfo1, Info), TreeWalker2}}
        end, {#ast_info{}, TreeWalker}, ArgList),

    NewScope = lists:map(fun({{identifier, _, LocalVarName}, _Value}) ->
                    {LocalVarName, erl_syntax:variable(lists:concat(["Var_", LocalVarName]))}
            end, ArgList),

    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(Contents,
        Context#dtl_context{local_scopes = [NewScope|Context#dtl_context.local_scopes]}, TreeWalker1),

    {{erl_syntax:application(
                erl_syntax:fun_expr([
            erl_syntax:clause(lists:map(fun({_, Var}) -> Var end, NewScope), none,
                [InnerAst])]), ArgAstList), merge_info(ArgInfo, InnerInfo)}, TreeWalker2}.

for_loop_ast(IteratorList, LoopValue, Contents, {EmptyContentsAst, EmptyContentsInfo}, Context, TreeWalker) ->
    Vars = lists:map(fun({identifier, _, Iterator}) -> 
                erl_syntax:variable(lists:concat(["Var_", Iterator])) 
            end, IteratorList),
    {{InnerAst, Info}, TreeWalker1} = body_ast(Contents,
        Context#dtl_context{local_scopes = [
                [{'forloop', erl_syntax:variable("Counters")} | lists:map(
                    fun({identifier, _, Iterator}) ->
                            {Iterator, erl_syntax:variable(lists:concat(["Var_", Iterator]))} 
                    end, IteratorList)] | Context#dtl_context.local_scopes]}, TreeWalker),
    CounterAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
        erl_syntax:atom(increment_counter_stats), [erl_syntax:variable("Counters")]),

    {{LoopValueAst, LoopValueInfo}, TreeWalker2} = value_ast(LoopValue, false, Context, TreeWalker1),

    CounterVars0 = case resolve_scoped_variable_ast('forloop', Context) of
        undefined ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [LoopValueAst]);
        Value ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [LoopValueAst, Value])
    end,
    {{erl_syntax:case_expr(
                    erl_syntax:application(
                            erl_syntax:atom('lists'), erl_syntax:atom('mapfoldl'),
                            [erl_syntax:fun_expr([
                                        erl_syntax:clause([erl_syntax:tuple(Vars), erl_syntax:variable("Counters")], none, 
                                            [erl_syntax:tuple([InnerAst, CounterAst])]),
                                        erl_syntax:clause(case Vars of [H] -> [H, erl_syntax:variable("Counters")];
                                                _ -> [erl_syntax:list(Vars), erl_syntax:variable("Counters")] end, none, 
                                            [erl_syntax:tuple([InnerAst, CounterAst])])
                                    ]),
                                CounterVars0, LoopValueAst]),
                        [erl_syntax:clause(
                                [erl_syntax:tuple([erl_syntax:underscore(), 
                                    erl_syntax:list([erl_syntax:tuple([erl_syntax:atom(counter), erl_syntax:integer(1)])], 
                                        erl_syntax:underscore())])],
                                none, [EmptyContentsAst]),
                            erl_syntax:clause(
                                [erl_syntax:tuple([erl_syntax:variable("L"), erl_syntax:underscore()])],
                                none, [erl_syntax:variable("L")])]
                    ),
                    merge_info(merge_info(Info, EmptyContentsInfo), LoopValueInfo)
            }, TreeWalker2}.

cycle_ast(Names, Context, TreeWalker) ->
    NamesTuple = lists:map(fun({string_literal, _, Str}) ->
                                   erl_syntax:string(unescape_string_literal(Str));
                              ({variable, _}=Var) ->
                                   {V, _} = resolve_variable_ast(Var, Context),
                                   V;
                              ({number_literal, _, Num}) ->
                                   format(erl_syntax:integer(Num), Context, TreeWalker);
                              (_) ->
                                   []
                           end, Names),
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters")]), #ast_info{}}, TreeWalker}.

%% Older Django templates treat cycle with comma-delimited elements as strings
cycle_compat_ast(Names, _Context, TreeWalker) ->
    NamesTuple = [erl_syntax:string(X) || {identifier, _, X} <- Names],
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters")]), #ast_info{}}, TreeWalker}.

now_ast(FormatString, _Context, TreeWalker) ->
    % Note: we can't use unescape_string_literal here
    % because we want to allow escaping in the format string.
    % We only want to remove the surrounding escapes,
    % i.e. \"foo\" becomes "foo"
    UnescapeOuter = string:strip(FormatString, both, 34),
    {{erl_syntax:application(
        erl_syntax:atom(erlydtl_dateformat),
        erl_syntax:atom(format),
        [erl_syntax:string(UnescapeOuter)]),
        #ast_info{}}, TreeWalker}.

spaceless_ast(Contents, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = body_ast(Contents, Context, TreeWalker),
    {{erl_syntax:application(
                erl_syntax:atom(erlydtl_runtime),
                erl_syntax:atom(spaceless),
                [Ast]), Info}, TreeWalker1}.

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).


full_path(File, DocRoot) ->
    case filename:absname(File) of
        File -> File;
        _ -> filename:join([DocRoot, File])
    end.
        
%%-------------------------------------------------------------------
%% Custom tags
%%-------------------------------------------------------------------

tag_ast(Name, Args, Context, TreeWalker) ->
    {InterpretedArgs, AstInfo} = lists:mapfoldl(fun
            ({{identifier, _, Key}, {string_literal, _, Value}}, AstInfoAcc) ->
                {erl_syntax:tuple([erl_syntax:string(Key), erl_syntax:string(unescape_string_literal(Value))]), AstInfoAcc};
            ({{identifier, _, Key}, Value}, AstInfoAcc) ->
                {AST, VarName} = resolve_variable_ast(Value, Context),
                {erl_syntax:tuple([erl_syntax:string(Key), format(AST,Context, TreeWalker)]), merge_info(#ast_info{var_names=[VarName]}, AstInfoAcc)}
        end, #ast_info{}, Args),

    {RenderAst, RenderInfo} = case Context#dtl_context.custom_tags_module of
        none ->
            {erl_syntax:application(none, erl_syntax:atom(render_tag),
                [erl_syntax:string(Name), erl_syntax:list(InterpretedArgs), erl_syntax:variable("TranslationFun")]),
            AstInfo#ast_info{custom_tags = [Name]}};
        Module ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
                [erl_syntax:list(InterpretedArgs), erl_syntax:variable("TranslationFun")]), AstInfo}
    end,
    {{RenderAst, RenderInfo}, TreeWalker}.

call_ast(Module, TreeWalkerAcc) ->
    call_ast(Module, erl_syntax:variable("Variables"), #ast_info{}, TreeWalkerAcc).

call_with_ast(Module, Variable, Context, TreeWalker) ->
    {VarAst, VarName} = resolve_variable_ast(Variable, Context),
    call_ast(Module, VarAst, #ast_info{var_names=[VarName]}, TreeWalker).
        
call_ast(Module, Variable, AstInfo, TreeWalker) ->
     AppAst = erl_syntax:application(
		erl_syntax:atom(Module),
		erl_syntax:atom(render),
                [Variable, erl_syntax:variable("TranslationFun")]),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
	      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
	      none,
	      [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
		  erl_syntax:atom(io_lib),
		  erl_syntax:atom(format),
		  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
		 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
		 none,
		 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),   
    with_dependencies(Module:dependencies(), {{CallAst, AstInfo}, TreeWalker}).
