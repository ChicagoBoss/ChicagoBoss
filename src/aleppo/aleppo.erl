% Aleppo: ALternative Erlang Pre-ProcessOr 
-module(aleppo).
-export([process_file/1, process_tokens/1, process_tokens/2, scan_file/1]).

-record(ale_context, {
        include_trail = [],
        include_dirs = [],
        macro_dict = dict:new()
    }).

process_file(FileName) ->
    ModuleName = list_to_atom(filename:rootname(filename:basename(FileName))),
    case scan_file(FileName) of
        {ok, Tokens} ->
            process_tokens(Tokens, [{file, FileName}, {module, ModuleName}]);
        Error ->
            Error
    end.

process_tokens(Tokens) ->
    process_tokens(Tokens, []).

% Valid options:
% - file: The path of the file being processed
% - include: A list of directories to include in the .hrl search path
process_tokens(Tokens, Options) ->
    {Tokens1, Module} = mark_keywords(Tokens),
    case aleppo_parser:parse(Tokens1) of
        {ok, ParseTree} ->
            process_tree(ParseTree, [{module, Module}|Options]);
        Error ->
            Error
    end.

process_tree(ParseTree, Options) ->
    {Dict0, IncludeTrail, IncludeDirs, TokenAcc} = case proplists:get_value(file, Options) of
        undefined -> {dict:new(), [], ["."], []};
        FileName -> {dict:store('FILE', [{string, 1, FileName}], dict:new()),
                [filename:absname(FileName)], 
                [".", filename:dirname(FileName)],
                lists:reverse(file_attribute_tokens(FileName, 1))}
    end,

    Dict1 = case proplists:get_value(module, Options) of
        undefined -> Dict0;
        Module -> 
            dict:store('MODULE', [{atom, 1, Module}], 
                dict:store('MODULE_NAME', [{string, 1, atom_to_list(Module)}], Dict0))
    end,

    Dict2 = dict:store('MACHINE',  [{atom, 1, list_to_atom(erlang:system_info(machine))}], Dict1),

    Context = #ale_context{ 
        include_trail = IncludeTrail, 
        include_dirs = IncludeDirs ++ proplists:get_value(include, Options, []), 
        macro_dict = Dict2 },

    case catch process_tree(ParseTree, TokenAcc, Context) of
        {error, What} ->
            {error, What};
        {_MacroDict, RevTokens} ->
            {ok, lists:reverse(RevTokens)}
    end.

process_tree([], TokenAcc, Context) ->
    {Context#ale_context.macro_dict, TokenAcc};
process_tree([Node|Rest], TokenAcc, Context) ->
    case Node of
        {'macro_define', {_Type, Loc, MacroName}} ->
            NewDict = dict:store(MacroName, [{atom,Loc,true}], Context#ale_context.macro_dict),
            process_tree(Rest, TokenAcc, Context#ale_context{ macro_dict = NewDict });
        {'macro_define', {_Type, _Loc, MacroName}, MacroTokens} ->
            NewDict = dict:store(MacroName, MacroTokens, Context#ale_context.macro_dict),
            process_tree(Rest, TokenAcc, Context#ale_context{ macro_dict = NewDict });
        {'macro_define', {_Type, _Loc, MacroName}, MacroArgs, MacroTokens} ->
            NewDict = dict:store({MacroName, length(MacroArgs)}, {MacroArgs, MacroTokens}, Context#ale_context.macro_dict),
            process_tree(Rest, TokenAcc, Context#ale_context{ macro_dict = NewDict });
        {'macro_undef', {_Type, _Loc, MacroName}} ->
            NewDict = dict:erase(MacroName, Context#ale_context.macro_dict),
            process_tree(Rest, TokenAcc, Context#ale_context{ macro_dict = NewDict });
        {'macro_include', {string, Loc, FileName}} ->
            AbsName = expand_filename(FileName, Context),
            {NewDict, IncludeTokens} = process_inclusion(AbsName, Loc, Context),
            process_tree(Rest, IncludeTokens ++ TokenAcc, Context#ale_context{ macro_dict = NewDict });
        {'macro_include_lib', {string, Loc, FileName}} ->
            AbsName = expand_include_lib(FileName),
            {NewDict, IncludeTokens} = process_inclusion(AbsName, Loc, Context),
            process_tree(Rest, IncludeTokens ++ TokenAcc, Context#ale_context{ macro_dict = NewDict });
        {'macro_ifdef', {_Type, _Loc, MacroName}, IfBody} ->
            process_ifelse(Rest, MacroName, IfBody, [], TokenAcc, Context);
        {'macro_ifdef', {_Type, _Loc, MacroName}, IfBody, ElseBody} ->
            process_ifelse(Rest, MacroName, IfBody, ElseBody, TokenAcc, Context);
        {'macro_ifndef', {_Type, _Loc, MacroName}, IfBody} ->
            process_ifelse(Rest, MacroName, [], IfBody, TokenAcc, Context);
        {'macro_ifndef', {_Type, _Loc, MacroName}, IfBody, ElseBody} ->
            process_ifelse(Rest, MacroName, ElseBody, IfBody, TokenAcc, Context);
        {'macro', {var, {Line, _Col} = Loc, 'LINE'}} ->
            process_tree(Rest, [{integer, Loc, Line}|TokenAcc], Context);
        {'macro', {var, Line, 'LINE'}} when is_integer(Line) ->
            process_tree(Rest, [{integer, Line, Line}|TokenAcc], Context);
        {'macro', {_Type, _Loc, MacroName}} ->
            InsertTokens = dict:fetch(MacroName, Context#ale_context.macro_dict),
            {_, RevProcessedTokens} = process_tree(InsertTokens, [], Context),
            process_tree(Rest, RevProcessedTokens ++ TokenAcc, Context);
        {'macro', {_Type, Loc, MacroName}, MacroArgs} ->
            {DefinedArgs, DefinedTokens} = dict:fetch({MacroName, length(MacroArgs)}, Context#ale_context.macro_dict),
            InsertTokens = expand_macro_fun(Loc, DefinedArgs, DefinedTokens, MacroArgs),
            {_, RevProcessedTokens} = process_tree(InsertTokens, [], Context),
            process_tree(Rest, RevProcessedTokens ++ TokenAcc, Context);
        OtherToken ->
            process_tree(Rest, [OtherToken|TokenAcc], Context)
    end.

process_ifelse(Rest, MacroName, IfBody, ElseBody, TokenAcc, Context) ->
    ChooseBody = case dict:is_key(MacroName, Context#ale_context.macro_dict) of
        true -> IfBody;
        false -> ElseBody
    end,
    {NewDict, NewTokens} = process_tree(ChooseBody, [], Context),
    process_tree(Rest, NewTokens ++ TokenAcc, Context#ale_context{ macro_dict = NewDict }).

process_inclusion(FileName, {Line, _}, Context) ->
    process_inclusion(FileName, Line, Context);
process_inclusion(FileName, Line, Context) ->
    case lists:member(FileName, Context#ale_context.include_trail) of
        true ->
            throw({error, {circular_inclusion, FileName}});
        false ->
            {ok, Tokens} = scan_file(FileName),
            {NewTokens, _} = mark_keywords(Tokens),
            case aleppo_parser:parse(NewTokens) of
                {ok, ParseTree} ->
                    [{eof, _}|Rest] = lists:reverse(ParseTree),
                    ParseTreeNoEOF = lists:reverse(Rest),
                    ThisFile = case dict:find('FILE', Context#ale_context.macro_dict) of
                        {ok, Val} -> Val;
                        _ -> undefined
                    end,
                    Dict1 = dict:store('FILE', [{string, 1, FileName}], Context#ale_context.macro_dict),
                    TokenAcc = lists:reverse(file_attribute_tokens(FileName, 1)),
                    {Dict2, IncludedTokens} = process_tree(ParseTreeNoEOF, TokenAcc, 
                        Context#ale_context{ 
                            macro_dict = Dict1, 
                            include_trail = [FileName|Context#ale_context.include_trail]}),
                    case ThisFile of
                        undefined -> {Dict2, IncludedTokens};
                        [{string, _Loc, ThisFileName}] -> 
                            {dict:store('FILE', ThisFile, Dict2),
                                lists:reverse(file_attribute_tokens(ThisFileName, Line)) ++ IncludedTokens}
                    end;
                Error ->
                    throw(Error)
            end
    end.

file_attribute_tokens(FileName, Line) ->
    [{'-', Line}, {atom, Line, 'file'}, {'(', Line}, {string, Line, FileName}, {',', Line},
        {integer, Line, Line}, {')', Line}, {dot, Line}].

expand_include_lib(FileName) ->
    [Lib | Rest] = filename:split(FileName),
    filename:join([code:lib_dir(list_to_atom(Lib))|Rest]).

expand_filename([$/|_] = FileName, _) ->
    case filelib:is_file(FileName) of
        true -> FileName;
        false -> throw({error, {not_found, FileName}})
    end;
expand_filename([$$|FileNameMinusDollar] = FileName, Context) ->
    [Var | Rest] = filename:split(FileNameMinusDollar),
    case os:getenv(Var) of
        false ->
            expand_relative_filename(FileName, Context);
        Value ->
            expand_filename(filename:join([Value|Rest]), Context)
    end;
expand_filename(FileName, Context) ->
    expand_relative_filename(FileName, Context).

expand_relative_filename(FileName, Context) ->
    ExpandedFileName = lists:foldl(
        fun
            (Dir, "") ->
                ExpandedFileName = filename:join(Dir, FileName),
                case filelib:is_file(ExpandedFileName) of
                    true -> ExpandedFileName;
                    false -> ""
                end;
            (_, F) ->
                F
        end, "", Context#ale_context.include_dirs),
    case ExpandedFileName of
        "" -> throw({error, {not_found, FileName}});
        ExpandedFileName ->
            filename:absname(ExpandedFileName)
    end.

scan_file(FileName) ->
    {ok, FileContents} = file:read_file(FileName),
    Data = binary_to_list(FileContents),
    scan_tokens(Data).

scan_tokens(Data) ->
    scan_tokens(Data, {1, 1}).

scan_tokens(Data, StartLocation) ->
    case erl_scan:tokens([], Data, StartLocation) of
        {done, Return, Rest} ->
            case Return of
                {ok, Tokens, EndLocation} ->
                    case scan_tokens(Rest, EndLocation) of
                        {ok, NewTokens} ->
                            {ok, Tokens ++ NewTokens};
                        Err -> Err
                    end;
                {eof, EndLocation} ->
                    {ok, [{eof, EndLocation}]};
                {error, ErrorInfo, _EndLocation} ->
                    {error, ErrorInfo}
            end;
        {more, Continuation1} ->
            {done, Return, eof} = erl_scan:tokens(Continuation1, eof, eof),
            case Return of
                {ok, Tokens, _EndLocation} ->
                    {ok, Tokens};
                {eof, EndLocation} ->
                    {ok, [{eof, EndLocation}]};
                {error, ErrorInfo, _EndLocation} ->
                    {error, ErrorInfo}
            end
    end.

expand_macro_fun(Loc, DefinedArgs, DefinedTokens, ApplyArgs) ->
    ExpandedTokens = replace_macro_strings(DefinedTokens, DefinedArgs, ApplyArgs),
    DefinedArgsWithCommas = insert_comma_tokens(DefinedArgs, Loc),
    ApplyArgsWithCommas = insert_comma_tokens(ApplyArgs, Loc),
    [{'(', Loc}, {'fun', Loc}, {'(', Loc}|DefinedArgsWithCommas] ++
        [{')', Loc}, {'->', Loc}|ExpandedTokens] ++
        [{'end', Loc}, {')', Loc}, {'(', Loc}|ApplyArgsWithCommas] ++
        [{')', Loc}].

replace_macro_strings(DefinedTokens, DefinedArgs, ApplyArgs) ->
    MacroStringDict = dict:from_list(lists:zipwith(fun([{var, _, VarName}], ApplyTokens) ->
                    ArgAsString = lists:concat(lists:foldr(
                            fun
                                (Token, []) -> 
                                    {symbol, Symbol} = erl_scan:token_info(Token, symbol),
                                    [Symbol];
                                (Token, Acc) -> 
                                    {symbol, Symbol} = erl_scan:token_info(Token, symbol),
                                    [Symbol, " "|Acc]
                            end, [], ApplyTokens)),
                    {VarName, ArgAsString}
            end, DefinedArgs, ApplyArgs)),
    replace_macro_strings1(DefinedTokens, MacroStringDict, []).

replace_macro_strings1([], _, Acc) ->
    lists:reverse(Acc);
replace_macro_strings1([{'macro_string', {var, Loc, VarName}}|Rest], MacroStringDict, Acc) ->
    replace_macro_strings1(Rest, MacroStringDict, [{string, Loc, dict:fetch(VarName, MacroStringDict)}|Acc]);
replace_macro_strings1([OtherToken|Rest], MacroStringDict, Acc) ->
    replace_macro_strings1(Rest, MacroStringDict, [OtherToken|Acc]).

insert_comma_tokens(Args, Loc) ->
    lists:foldr(fun
            (Arg, []) -> Arg;
            (Arg, Acc) -> Arg ++ [{',', Loc}|Acc]
        end, [], Args).

mark_keywords(Tokens) ->
    mark_keywords(Tokens, undefined, []).

mark_keywords([], Module, Acc) ->
    {lists:reverse(Acc), Module};
mark_keywords([{'-', {DashLine, 1}} = Dash, {atom, {DashLine, 2}, 'module'} = Token,
        {'(', {DashLine, _}} = Paren, {atom, {DashLine, _}, ModuleName} = ModToken|Rest], _, Acc) ->
    mark_keywords(Rest, ModuleName, [ModToken, Paren, Token, Dash|Acc]);
mark_keywords([{'-', {DashLine, 1}} = Dash, {atom, {DashLine, 2} = AtomLoc, Atom} = Token|Rest], Mod, Acc) ->
    MarkedToken = case Atom of
        'define' -> {define_keyword, AtomLoc};
        'ifdef' -> {ifdef_keyword, AtomLoc};
        'ifndef' -> {ifndef_keyword, AtomLoc};
        'else' -> {else_keyword, AtomLoc};
        'endif' -> {endif_keyword, AtomLoc};
        'undef' -> {undef_keyword, AtomLoc};
        'include' -> {include_keyword, AtomLoc};
        'include_lib' -> {include_lib_keyword, AtomLoc};
        _ -> Token
    end,
    mark_keywords(Rest, Mod, [MarkedToken, Dash|Acc]);
mark_keywords([Other|Rest], Mod, Acc) ->
    mark_keywords(Rest, Mod, [Other|Acc]).
