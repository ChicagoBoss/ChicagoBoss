% Aleppo: ALternative Erlang Pre-ProcessOr 
-module(aleppo).
-export([process_file/1, process_tokens/1, process_tokens/2, scan_file/1]).

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

process_tokens(Tokens, Options) ->
    {Tokens1, Module} = mark_keywords(Tokens),
    case aleppo_parser:parse(Tokens1) of
        {ok, ParseTree} ->
            process_tree(ParseTree, [{module, Module}|Options]);
        Error ->
            Error
    end.

process_tree(ParseTree, Options) ->
    {Dict0, InclusionTrail, TokenAcc} = case proplists:get_value(file, Options) of
        undefined -> {dict:new(), [], []};
        FileName -> {dict:store('FILE', [{string, 1, FileName}], dict:new()),
                [filename:absname(FileName)], 
                lists:reverse(file_attribute_tokens(FileName, 1))}
    end,

    Dict1 = case proplists:get_value(module, Options) of
        undefined -> Dict0;
        Module -> 
            dict:store('MODULE', [{atom, 1, Module}], 
                dict:store('MODULE_NAME', [{string, 1, atom_to_list(Module)}], Dict0))
    end,

    Dict2 = dict:store('MACHINE',  [{atom, 1, list_to_atom(erlang:system_info(machine))}], Dict1),

    case catch process_tree(ParseTree, Dict2, InclusionTrail, TokenAcc) of
        {error, What} ->
            {error, What};
        {_MacroDict, RevTokens} ->
            {ok, lists:reverse(RevTokens)}
    end.

process_tree([], MacroDict, _, TokenAcc) ->
    {MacroDict, TokenAcc};
process_tree([Node|Rest], MacroDict, InclusionTrail, TokenAcc) ->
    case Node of
        {'macro_define', {_Type, Loc, MacroName}} ->
            NewDict = dict:store(MacroName, [{atom,Loc,true}], MacroDict),
            process_tree(Rest, NewDict, InclusionTrail, TokenAcc);
        {'macro_define', {_Type, _Loc, MacroName}, MacroTokens} ->
            NewDict = dict:store(MacroName, MacroTokens, MacroDict),
            process_tree(Rest, NewDict, InclusionTrail, TokenAcc);
        {'macro_define', {_Type, _Loc, MacroName}, MacroArgs, MacroTokens} ->
            NewDict = dict:store({MacroName, length(MacroArgs)}, {MacroArgs, MacroTokens}, MacroDict),
            process_tree(Rest, NewDict, InclusionTrail, TokenAcc);
        {'macro_undef', {_Type, _Loc, MacroName}} ->
            NewDict = dict:erase(MacroName, MacroDict),
            process_tree(Rest, NewDict, InclusionTrail, TokenAcc);
        {'macro_include', {string, Loc, FileName}} ->
            AbsName = filename:absname(FileName),
            {IncludeMacroDict, IncludeTokens} = process_inclusion(AbsName, Loc, MacroDict, InclusionTrail),
            process_tree(Rest, IncludeMacroDict, InclusionTrail, IncludeTokens ++ TokenAcc);
        {'macro_include_lib', {string, Loc, FileName}} ->
            AbsName = expand_include_lib(FileName),
            {IncludeMacroDict, IncludeTokens} = process_inclusion(AbsName, Loc, MacroDict, InclusionTrail),
            process_tree(Rest, IncludeMacroDict, InclusionTrail, IncludeTokens ++ TokenAcc);
        {'macro_ifdef', {_Type, _Loc, MacroName}, IfBody} ->
            process_ifelse(Rest, MacroName, IfBody, [], MacroDict, InclusionTrail, TokenAcc);
        {'macro_ifdef', {_Type, _Loc, MacroName}, IfBody, ElseBody} ->
            process_ifelse(Rest, MacroName, IfBody, ElseBody, MacroDict, InclusionTrail, TokenAcc);
        {'macro_ifndef', {_Type, _Loc, MacroName}, IfBody} ->
            process_ifelse(Rest, MacroName, [], IfBody, MacroDict, InclusionTrail, TokenAcc);
        {'macro_ifndef', {_Type, _Loc, MacroName}, IfBody, ElseBody} ->
            process_ifelse(Rest, MacroName, ElseBody, IfBody, MacroDict, InclusionTrail, TokenAcc);
        {'macro', {var, {Line, _Col} = Loc, 'LINE'}} ->
            process_tree(Rest, MacroDict, InclusionTrail, [{integer, Loc, Line}|TokenAcc]);
        {'macro', {var, Line, 'LINE'}} when is_integer(Line) ->
            process_tree(Rest, MacroDict, InclusionTrail, [{integer, Line, Line}|TokenAcc]);
        {'macro', {_Type, _Loc, MacroName}} ->
            InsertTokens = dict:fetch(MacroName, MacroDict),
            process_tree(Rest, MacroDict, InclusionTrail, lists:reverse(InsertTokens) ++ TokenAcc);
        {'macro', {_Type, Loc, MacroName}, MacroArgs} ->
            {DefinedArgs, DefinedTokens} = dict:fetch({MacroName, length(MacroArgs)}, MacroDict),
            InsertTokens = expand_macro_fun(Loc, DefinedArgs, DefinedTokens, MacroArgs),
            process_tree(Rest, MacroDict, InclusionTrail, lists:reverse(InsertTokens) ++ TokenAcc);
        OtherToken ->
            process_tree(Rest, MacroDict, InclusionTrail, [OtherToken|TokenAcc])
    end.

process_ifelse(Rest, MacroName, IfBody, ElseBody, MacroDict, InclusionTrail, TokenAcc) ->
    ChooseBody = case dict:is_key(MacroName, MacroDict) of
        true -> IfBody;
        false -> ElseBody
    end,
    {NewMacroDict, NewTokens} = process_tree(ChooseBody, MacroDict, InclusionTrail, []),
    process_tree(Rest, NewMacroDict, InclusionTrail, NewTokens ++ TokenAcc).

process_inclusion(FileName, {Line, _}, MacroDict, InclusionTrail) ->
    process_inclusion(FileName, Line, MacroDict, InclusionTrail);
process_inclusion(FileName, Line, MacroDict, InclusionTrail) ->
    case lists:member(FileName, InclusionTrail) of
        true ->
            throw({error, {circular_inclusion, FileName}});
        false ->
            {ok, Tokens} = scan_file(FileName),
            {NewTokens, _} = mark_keywords(Tokens),
            case aleppo_parser:parse(NewTokens) of
                {ok, ParseTree} ->
                    ThisFile = case dict:find('FILE', MacroDict) of
                        {ok, Val} -> Val;
                        _ -> undefined
                    end,
                    Dict1 = dict:store('FILE', [{string, 1, FileName}], MacroDict),
                    TokenAcc = lists:reverse(file_attribute_tokens(FileName, 1)),
                    {Dict2, IncludedTokens} = process_tree(ParseTree, Dict1, 
                        [FileName|InclusionTrail], TokenAcc),
                    case ThisFile of
                        undefined -> {Dict2, IncludedTokens};
                        {string, _Loc, ThisFileName} -> 
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
                {eof, _EndLocation} ->
                    {ok, []};
                {error, ErrorInfo, _EndLocation} ->
                    {error, ErrorInfo}
            end;
        {more, Continuation1} ->
            {done, Return, eof} = erl_scan:tokens(Continuation1, eof, eof),
            case Return of
                {ok, Tokens, _EndLocation} ->
                    {ok, Tokens};
                {eof, _EndLocation} ->
                    {ok, []};
                {error, ErrorInfo, _EndLocation} ->
                    {error, ErrorInfo}
            end
    end.

expand_macro_fun(Loc, DefinedArgs, DefinedTokens, ApplyArgs) ->
    [{'(', Loc}, {'fun', Loc}, {'(', Loc}|DefinedArgs] ++ % includes commas
        [{')', Loc}, {'->', Loc}|DefinedTokens] ++
        [{'end', Loc}, {')', Loc}, {'(', Loc}|ApplyArgs] ++
        [{')', Loc}].

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
