-module(boss_controller_compiler).
-compile(export_all).

compile(File) ->
    compile(File, [verbose, return_errors]).

compile(File, Options) ->
    case file:read_file(File) of
        {ok, FileContents} ->
            compile_text(File, FileContents, Options);
        Error ->
            Error
    end.

compile_text(FileName, FileContents, Options) ->
    case scan_transform(FileContents) of
        {ok, Tokens} ->
            {ok, ProcessedTokens} = aleppo:process_tokens(Tokens, [{file, FileName}]),
            {Forms, Errors} = parse_tokens(ProcessedTokens),
            case length(Errors) of
                0 ->
                    NewOptions = [{parse_transform, boss_db_pt}|Options],
                    case boss_load:compile_forms(Forms, FileName, NewOptions) of
                        {ok, ModuleName, Bin} ->
                            ok;
                        Error ->
                            Error
                    end;
                _ ->
                    {error, Errors, []}
            end;
        {error, Error} ->
            {error, Error}
    end.

parse_tokens(Tokens) ->
    parse_tokens(Tokens, [], [], []).

parse_tokens([], _, FormAcc, ErrorAcc) ->
    {lists:reverse(FormAcc), lists:reverse(ErrorAcc)};
parse_tokens([{dot, _}=Token|Rest], TokenAcc, FormAcc, ErrorAcc) ->
    case erl_parse:parse_form(lists:reverse([Token|TokenAcc])) of
        {ok, AbsForm} ->
            parse_tokens(Rest, [], [AbsForm|FormAcc], ErrorAcc);
        {error, ErrorInfo} ->
            parse_tokens(Rest, [], FormAcc, [ErrorInfo|ErrorAcc])
    end;
parse_tokens([Token|Rest], TokenAcc, FormAcc, ErrorAcc) ->
    parse_tokens(Rest, [Token|TokenAcc], FormAcc, ErrorAcc).

scan_transform(FileContents) ->
    scan_transform(FileContents, {1, 1}).

scan_transform([], _) ->
    {ok, []};
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
                {eof, _EndLocation} ->
                    {ok, []};
                {error, ErrorInfo, _EndLocation} ->
                    case ErrorInfo of
                        {ErrorLocation, erl_scan, {illegal,character}} ->
                            {Truncated, IllegalChar, Rest} = cut_at_location(ErrorLocation, FileContents, StartLocation),
                            case transform_char(IllegalChar) of
                                {ok, String} ->
                                    Transformed = Truncated ++ String ++ Rest,
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
                {error, ErrorInfo, _EndLocation} ->
                    {error, ErrorInfo}
            end
    end.

transform_char(8800) -> % ≠
    {ok, ",'not_equal',"};
transform_char(8804) -> % ≤
    {ok, ",'less_equal',"};
transform_char(8805) -> % ≥
    {ok, ",'greater_equal',"};
transform_char(8712) -> % ∈
    {ok, ",'in_list',"};
transform_char(8713) -> % ∉
    {ok, ",'not_in_list',"};
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
