-module(boss_compiler).
-export([compile/1, compile/2, parse/1]).

%% @spec compile( File::string() ) -> {ok, Module} | {error, Reason}
compile(File) ->
    compile(File, []).

compile(File, Options) ->
    IncludeDirs = proplists:get_value(include_dirs, Options, []),
    case parse(File, IncludeDirs) of
        {ok, Forms} ->
            CompilerOptions = proplists:get_value(compiler_options, Options, 
                [verbose, return_errors]),
            NewForms = case proplists:get_value(pre_revert_transform, Options) of
                undefined ->
                    Forms;
                TransformFun when is_function(TransformFun) ->
                    TransformFun(Forms)
            end,
            ParseTransforms = [boss_db_pt],
            RevertedForms = lists:foldl(fun(Mod, Acc) ->
                       Mod:parse_transform(Acc, CompilerOptions)
               end, erl_syntax:revert(NewForms), ParseTransforms),
            case compile_forms(RevertedForms, File, CompilerOptions) of
                {ok, Module, Bin} ->
                    ok = case proplists:get_value(out_dir, Options) of
                        undefined -> ok;
                        OutDir ->
                            BeamFile = filename:join([OutDir, lists:concat([Module, ".beam"])]),
                            file:write_file(BeamFile, Bin)
                    end,
                    {ok, Module};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

compile_forms(Forms, File, Options) ->
    case compile:forms(Forms, Options) of
        {ok, Module1, Bin} ->
            code:purge(Module1),
            case code:load_binary(Module1, File, Bin) of
                {module, _} -> {ok, Module1, Bin};
                _ -> {error, lists:concat(["code reload failed: ", Module1])}
            end;
        OtherError ->
            OtherError
    end.

parse(File) ->
    parse(File, []).

parse(File, IncludeDirs) ->
    case file:read_file(File) of
        {ok, FileContents} ->
            parse_text(File, FileContents, IncludeDirs);
        Error ->
            Error
    end.

parse_text(FileName, FileContents, IncludeDirs) ->
    case scan_transform(FileContents) of
        {ok, Tokens} ->
            case aleppo:process_tokens(Tokens, [{file, FileName}, {include, IncludeDirs}]) of
                {ok, ProcessedTokens} ->
                    {Forms, Errors} = parse_tokens(ProcessedTokens, FileName),
                    case length(Errors) of
                        0 ->
                            {ok, Forms};
                        _ ->
                            Errors1 = lists:map(fun(File) ->
                                        {File, proplists:get_all_values(File, Errors)}
                                end, proplists:get_keys(Errors)),
                            {error, Errors1, []}
                    end;
                {error, ErrorInfo} ->
                    {error, {FileName, [ErrorInfo]}}
            end;
        {error, ErrorInfo} ->
            {error, {FileName, [ErrorInfo]}}
    end.

parse_tokens(Tokens, FileName) ->
    parse_tokens(Tokens, [], [], [], FileName).

parse_tokens([], _, FormAcc, ErrorAcc, _) ->
    {lists:reverse(FormAcc), lists:reverse(ErrorAcc)};
parse_tokens([{dot, _}=Token|Rest], TokenAcc, FormAcc, ErrorAcc, FileName) ->
    case erl_parse:parse_form(lists:reverse([Token|TokenAcc])) of
        {ok, {attribute, _, file, {NewFileName, _Line}} = AbsForm} ->
            parse_tokens(Rest, [], [AbsForm|FormAcc], ErrorAcc, NewFileName);
        {ok, AbsForm} ->
            parse_tokens(Rest, [], [AbsForm|FormAcc], ErrorAcc, FileName);
        {error, ErrorInfo} ->
            parse_tokens(Rest, [], FormAcc, [{FileName, ErrorInfo}|ErrorAcc], FileName)
    end;
parse_tokens([{eof, Location}], TokenAcc, FormAcc, ErrorAcc, FileName) ->
    parse_tokens([], TokenAcc, [{eof, Location}|FormAcc], ErrorAcc, FileName);
parse_tokens([Token|Rest], TokenAcc, FormAcc, ErrorAcc, FileName) ->
    parse_tokens(Rest, [Token|TokenAcc], FormAcc, ErrorAcc, FileName).

scan_transform(FileContents) ->
    scan_transform(FileContents, {1, 1}).

scan_transform([], StartLocation) ->
    {ok, [{eof, StartLocation}]};
scan_transform(FileContents, StartLocation) when is_binary(FileContents) ->
    scan_transform(unicode:characters_to_list(FileContents), StartLocation);
scan_transform(FileContents, StartLocation) ->
    case erl_scan:tokens([], FileContents, StartLocation) of
        {done, Return, Rest} ->
            case Return of
                {ok, Tokens, EndLocation} ->
                    case scan_transform(Rest, EndLocation) of
                        {ok, NewTokens} ->
                            {ok, Tokens ++ NewTokens};
                        Err -> Err
                    end;
                {eof, EndLocation} ->
                    {ok, [{eof, EndLocation}]};
                {error, ErrorInfo, _EndLocation} ->
                    case ErrorInfo of
                        {ErrorLocation, erl_scan, {illegal,character}} ->
                            {Truncated, IllegalChar, Rest1} = cut_at_location(ErrorLocation, FileContents, StartLocation),
                            case transform_char(IllegalChar) of
                                {ok, String} ->
                                    Transformed = Truncated ++ String ++ Rest1,
                                    scan_transform(Transformed, StartLocation);
                                error ->
                                    {error, ErrorInfo}
                            end;
                        ErrorInfo ->
                            {error, ErrorInfo}
                    end
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

transform_char(8800) -> % ≠
    {ok, ",'not_equals',"};
transform_char(8804) -> % ≤
    {ok, ",'le',"};
transform_char(8805) -> % ≥
    {ok, ",'ge',"};
transform_char(8712) -> % ∈
    {ok, ",'in',"};
transform_char(8713) -> % ∉
    {ok, ",'not_in',"};
transform_char(8715) -> % ∋
    {ok, ",'contains',"};
transform_char(8716) -> % ∌
    {ok, ",'not_contains',"};
transform_char(8764) -> % ∼
    {ok, ",'matches',"};
transform_char(8769) -> % ≁
    {ok, ",'not_matches',"};
transform_char(8839) -> % ⊇
    {ok, ",'contains_all',"};
transform_char(8841) -> % ⊉
    {ok, ",'not_contains_all',"};
transform_char(8745) -> % ∩
    {ok, ",'contains_any',"};
transform_char(8869) -> % ⊥
    {ok, ",'contains_none',"};
transform_char(10178) -> % ⊥ look-alike
    {ok, ",'contains_none',"};
transform_char(Char) when Char > 127 ->
    {ok, lists:flatten(io_lib:format("\\x{~.16B}", [Char]))};
transform_char(_) ->
    error.

cut_at_location({CutLine, CutCol}, FileContents, {StartLine, StartCol}) ->
    cut_at_location1({CutLine, CutCol}, FileContents, {StartLine, StartCol}, []).

cut_at_location1(_, [], _, Acc) ->
    {lists:reverse(Acc), 0, ""};
cut_at_location1({Line, Col}, [C|Rest], {Line, Col}, Acc) ->
    {lists:reverse(Acc), C, Rest};
cut_at_location1({Line, Col}, [C|Rest], {ThisLine, _}, Acc) when C =:= $\n ->
    cut_at_location1({Line, Col}, Rest, {ThisLine + 1, 1}, [C|Acc]);
cut_at_location1({Line, Col}, [C|Rest], {ThisLine, ThisCol}, Acc) ->
    cut_at_location1({Line, Col}, Rest, {ThisLine, ThisCol + 1}, [C|Acc]).
