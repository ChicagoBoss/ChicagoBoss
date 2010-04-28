%%%-------------------------------------------------------------------
%%% File    : etcher_util.erl
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

-module(etcher_util).
-export([split/2,
         split/3,
         to_iolist/1,
         to_integer/1,
         to_float/1,
         to_string/1,
         to_ppstring/1,
         force_escape/1,
         conditional_escape/1,
         url_quote/1,
         url_quote/2,
         url_quote_plus/1,
         url_quote_plus/2,
         py_to_erl_fmt_string/1,
         trim/1,
         normalize_absolute_path/1,
         normalize_relative_path/1,
         hash/1
         ]).

-include("internal.hrl").

-define(IS_URI_UNRESERVED(C), (?IS_ALPHA_NUM(C) or 
                               (C =:= $-) or (C =:= $_) or 
                               (C =:= $.) or (C =:= $~))).

-record(qspec, {
            safe = [],
            quote_plus = false
            }).

-record(fmt_spec, {
            justify = right,
            field_width,
            precision,
            pad_char,
            type,
            expected_arg         % int | char | float | string | any
            }).

-record(split_opts, {
            return_type = binary,
            group = false,
            re_opts = []
            }).

%%------------------------------------------------------------------------
%% split/2,3
%%
%% Alternative to re:split/3. OTP versions R13B03 and below  have a bug 
%% which prevents re:split from handling the 'unicode' flag properly.
%% The problem is not with the PCRE implemenation - re:run works fine
%% with unicode. The bug is in the re:compile BIF which re:split calls
%% internally. The function contained here sidesteps the problem by 
%% calling re:run/3 without pre-compiling the regex is just stopgap. 
%%------------------------------------------------------------------------

split(Subject, RE) ->
    split(Subject, RE, []).

split(Subject, RE, Options) when (is_list(RE) or is_binary(RE)), 
                                         is_list(Options) ->
    FlatSubject =
        case lists:member(unicode, Options) of
            true ->
                unicode:characters_to_binary(Subject);
            false ->
                iolist_to_binary(Subject)
        end,
    SOpts = process_split_opts(Options),
    ReOpts = [global | SOpts#split_opts.re_opts],
    case re:run(Subject, RE, ReOpts) of
        {match, Matches} ->
            Splits = split_matches(Matches, 0, FlatSubject, []), 
            return_split_results(Splits, SOpts);
        nomatch ->
            Groups = [[FlatSubject]],
            return_split_results(Groups, SOpts)
    end.

split_matches([[{Index, Len} | SubExMatches] | Rest], Offset, Subject, Acc) ->
    NewOffset = Index + Len,
    InBetweenPart = cut_part(Offset, Index - Offset, Subject),
    SubExParts = cut_parts(SubExMatches, Subject, []),
    GroupedParts = [InBetweenPart | SubExParts],
    Acc1 = [GroupedParts | Acc],
    split_matches(Rest, NewOffset, Subject, Acc1);
split_matches([], Offset, Subject, Acc) ->
    TrailSize = byte_size(Subject) - Offset,
    TrailPart = cut_part(Offset, TrailSize, Subject),
    TrailGroup = [TrailPart],
    lists:reverse([TrailGroup | Acc]).

cut_parts([{-1, 0} | Rest], Bin, Acc) ->
    cut_parts(Rest, Bin, [<<>> | Acc]);
cut_parts([{Offset, Len} | Rest], Bin, Acc) ->
    Part = cut_part(Offset, Len, Bin),
    cut_parts(Rest, Bin, [Part | Acc]);
cut_parts([], _Bin, Acc) ->
    lists:reverse(Acc).

cut_part(Offset, Len, Bin) ->
    <<_:Offset/bytes, Part:Len/bytes, _/bytes>> = Bin,
    Part.

return_split_results(Splits, #split_opts{group=true} = SOpts) ->
    [prep_result_group(G, SOpts) || G <- Splits];
return_split_results(Splits, SOpts) ->
    [T || G <- Splits, T <- prep_result_group(G, SOpts)].

prep_result_group(Splits, #split_opts{return_type=list}) ->
    [unicode:characters_to_list(Bin) || Bin <- Splits];
prep_result_group(Splits, _) ->
    Splits.

process_split_opts(Options) ->
    process_opts(Options, #split_opts{}).

process_opts([{return, Type} | Rest], SOpts) ->
    RetType = 
        case Type of 
            iodata ->       % iodata is same as binary for us
                binary;
            binary ->  
                binary;
            list ->
                list;
            _ ->
                throw({unsupported_return_type, Type})
        end,
    SOpts1 = SOpts#split_opts{return_type=RetType},
    process_opts(Rest, SOpts1);
process_opts([group | Rest], SOpts) ->
    SOpts1 = SOpts#split_opts{group=true},
    process_opts(Rest, SOpts1);
process_opts([trim | _], _SOpts) ->
    throw(trim_option_not_supported);
process_opts([{parts, _} | _], _SOpts) ->
    throw(parts_option_not_supported);
process_opts([CaptOpt | _], _SOpts) when element(1, CaptOpt) =:= capture ->
    throw({bad_option, CaptOpt});
process_opts([Opt | Rest], #split_opts{re_opts=ReOpts} = SOpts) ->
    ReOpts1 = [Opt | ReOpts],
    SOpts1 = SOpts#split_opts{re_opts=ReOpts1},
    process_opts(Rest, SOpts1);
process_opts([], SOpts) ->
    SOpts.

%%------------------------------------------------------------------------
%% Type Conversions
%%------------------------------------------------------------------------

to_iolist(I) when is_integer(I) ->
    integer_to_list(I);
to_iolist(F) when is_float(F) ->
    float_to_list(F);
to_iolist(B) when is_binary(B) ->
    B;
to_iolist(#string{val=S}) ->
    to_iolist(S);
to_iolist(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        Bin when is_binary(Bin) ->
            Bin;
        _ ->
            ""
    end;
to_iolist(_) ->
    "".

to_integer(Integer) when is_integer(Integer) ->
    Integer;
to_integer(Float) when is_float(Float) ->
    round(Float);
to_integer(#string{val=S}) ->
    to_integer(S);
to_integer(RawString) when is_list(RawString) ->
    list_to_integer(trim(RawString));
to_integer(Bin) when is_binary(Bin) ->
    S = unicode:characters_to_list(Bin),
    to_integer(S).

to_float(Float) when is_float(Float) ->
    Float;
to_float(Integer) when is_integer(Integer) ->
    erlang:float(Integer);
to_float(#string{val=S}) ->
    to_float(S);
to_float(RawString) when is_list(RawString) ->
    S = trim(RawString),
    try list_to_float(S)
    catch 
        error:badarg ->
            I = list_to_integer(S),
            erlang:float(I)
    end;
to_float(Bin) when is_binary(Bin) ->
    S = unicode:characters_to_list(Bin),
    to_float(S).

to_string(#string{} = String) ->
    String;
to_string(I) when is_integer(I) ->
    #string{val=integer_to_list(I), safe=true};
to_string(F) when is_float(F) ->
    #string{val=float_to_list(F), safe=true};
to_string(T) when is_binary(T); ?IS_STRING(T) ->
    case unicode:characters_to_list(T) of
        S when is_list(S) ->
            #string{val=S};
        _Err ->
            ?EMPTY_STRING
    end;
to_string(T) ->
    #string{val=to_ppstring(T)}.

to_ppstring(T) ->
    lists:flatten(io_lib:format("~p", [T])).

%%------------------------------------------------------------------------
%% XML/HTML Escaping
%%------------------------------------------------------------------------

conditional_escape(#string{safe=true} = String) ->
    String;
conditional_escape(#string{} = String) ->
    force_escape(String).

force_escape(#string{val=S}) ->
    S1 = force_escape(S, []),
    #string{val=S1, safe=true}.

force_escape([$< | Rest], Acc) ->
    force_escape(Rest, ["&lt;" | Acc]); 
force_escape([$> | Rest], Acc) ->
    force_escape(Rest, ["&gt;" | Acc]); 
force_escape([$' | Rest], Acc) ->
    force_escape(Rest, ["&#39;" | Acc]); 
force_escape([$" | Rest], Acc) ->
    force_escape(Rest, ["&quot;" | Acc]); 
force_escape([$& | Rest], Acc) ->
    force_escape(Rest, ["&amp;" | Acc]); 
force_escape([C | Rest], Acc) ->
    force_escape(Rest, [C | Acc]); 
force_escape([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

%%------------------------------------------------------------------------
%% URL Encoding
%%------------------------------------------------------------------------

% Same idea as Python's urllib.quote(). 
% Used to encode the path part of a URL.
% http://docs.python.org/library/urllib.html#urllib.quote 
url_quote(S) ->
    url_quote(S, "/").

url_quote(S, Safe) when is_list(Safe) ->
    Qspec = #qspec{safe=Safe},
    quote(S, Qspec).

% Same idea as Python's urllib.quote_plus(). 
% Used to encode form data.
% http://docs.python.org/library/urllib.html#urllib.quote_plus
url_quote_plus(S) ->
    url_quote(S, "").

url_quote_plus(S, Safe) when is_list(Safe) ->
    Qspec = #qspec{safe=Safe, quote_plus=true},
    quote(S, Qspec).

quote(S, Qspec) ->
    Utf8Bin = unicode:characters_to_binary(S),
    quote(Utf8Bin, Qspec, []).

quote(<<C, Rest/bytes>>, Qspec, Acc) when ?IS_URI_UNRESERVED(C) ->
    quote(Rest, Qspec, [C | Acc]);
quote(<<$\s, Rest/bytes>>, #qspec{quote_plus=true} = Qspec, Acc) ->
    quote(Rest, Qspec, [$+ | Acc]);
quote(<<C, Rest/bytes>>, #qspec{safe=Safe} = Qspec, Acc) ->
    Acc1 = 
        case lists:member(C, Safe) of
            true ->
                [C | Acc];
            false ->
                HexStr = to_hexpair(C),
                lists:reverse(HexStr, Acc)
        end,
    quote(Rest, Qspec, Acc1);
quote(<<>>, _Qspec, Acc) ->
    lists:reverse(Acc).

to_hexpair(Byte) ->
    case erlang:integer_to_list(Byte, 16) of
        [C] ->
            [$%, $0, C];
        HexStr ->
            [$% | HexStr]
    end.

%%------------------------------------------------------------------------
%% Conversion of Python Format Strings
%%------------------------------------------------------------------------

% Python String Format Reference:
% http://docs.python.org/library/stdtypes.html#string-formatting-operations

py_to_erl_fmt_string(S) ->
    case parse_py_fmt_string(S, []) of
        {ok, ParsedData} ->
            ErlFmtString = to_erl_fmt_string(ParsedData, []),
            ExpectedArgs = expected_args(ParsedData),
            {ok, ErlFmtString, ExpectedArgs};
        {failed, _Reason} = Failure ->
            Failure
    end.

parse_py_fmt_string([$% | _] = S, Acc) ->
    case parse_py_fmt_spec(S) of
        {ok, FmtSpec, Rest} ->
            parse_py_fmt_string(Rest, [FmtSpec | Acc]);
        Failure ->
            Failure
    end;
parse_py_fmt_string([$~ | Rest], Acc) ->
    parse_py_fmt_string(Rest, [$~, $~ | Acc]);
parse_py_fmt_string([C | Rest], Acc) ->
    parse_py_fmt_string(Rest, [C | Acc]);
parse_py_fmt_string([], Acc) ->
    {ok, lists:reverse(Acc)}.

expected_args(ParsedFmtStr) ->
    [Expected || #fmt_spec{expected_arg=Expected} <- ParsedFmtStr].

to_erl_fmt_string([#fmt_spec{} = FS | Rest], Acc) ->
    S = serialize(FS),
    to_erl_fmt_string(Rest, [S | Acc]);
to_erl_fmt_string([C | Rest], Acc) ->
    to_erl_fmt_string(Rest, [C | Acc]);
to_erl_fmt_string([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

serialize(#fmt_spec{justify=Justify, field_width=FieldWidth,
                    precision=Precision, pad_char=PadChar, type=Type}) ->
    L = case {FieldWidth, Precision, PadChar} of
            {undefined, undefined, undefined} ->
                "";
            {undefined, undefined, _} ->
                ["..", PadChar];
            {undefined, _, undefined} ->
                [".", Precision];
            {undefined, _, _} ->
                [".", Precision, ".", PadChar];
            {_, undefined, undefined} ->
                FieldWidth;
            {_, undefined, _} ->
                [FieldWidth, "..", PadChar];
            {_, _, undefined} ->
                [FieldWidth, ".", Precision];
            {_, _, _} ->
                [FieldWidth, ".", Precision, ".", PadChar]
        end,
    L1 = 
        case Justify of
            left ->
                ["-", L];
            right ->
                L
        end,
    FmtStr = [$~, L1, Type],
    lists:flatten(FmtStr).

parse_py_fmt_spec(S) ->
    {Fields, Regex} = py_fmt_spec_regex(),
    case re:run(S, Regex, [unicode, dotall, {capture, all_but_first, list}]) of
        {match, Matches} ->
            parse_py_fmt_spec(Fields, Matches, #fmt_spec{});
        nomatch ->
            {failed, invalid_fmt_string}
    end.

parse_py_fmt_spec([mapkey | RestFields], [Text | RestMatches], FS) ->
    case Text of
        "" ->
            parse_py_fmt_spec(RestFields, RestMatches, FS);
        _ ->
            {failed, mapkeys_not_supported}
    end;
parse_py_fmt_spec([flags | RestFields], [Text | RestMatches], FS) ->
    case handle_fmt_flags(Text, FS) of
        {ok, FS1} ->
            parse_py_fmt_spec(RestFields, RestMatches, FS1);
        Failure ->
            Failure
    end;
parse_py_fmt_spec([min_field_width | RestFields], [Text | RestMatches], FS) ->
    FS1 = 
        case Text of
            "" ->
                % Can't justify left when there's no specified field width
                FS#fmt_spec{justify=right};
            _ ->
                FS#fmt_spec{field_width=Text}
        end,
    parse_py_fmt_spec(RestFields, RestMatches, FS1);
parse_py_fmt_spec([precision | RestFields], [Text | RestMatches], FS) ->
    FS1 = 
        case Text of
            "" ->
                FS;
            [$. | Text1] ->
                FS#fmt_spec{precision=Text1}
        end,
    parse_py_fmt_spec(RestFields, RestMatches, FS1);
parse_py_fmt_spec([type | RestFields], 
                  [Text | RestMatches], 
                  #fmt_spec{precision=Precision} = FS) ->
    {EFmtType, ExpectedArg, Precision1} =
        case Text of
            "d" -> {"b", int, Precision};
            "i" -> {"b", int, Precision};
            "o" -> {"b", int, "8"};
            "u" -> {"b", int, Precision};
            "x" -> {"b", int, "16"};
            "X" -> {"B", int, "16"};
            "e" -> {"e", float, Precision};
            "E" -> {"e", float, Precision};        % No uppercase version
            "f" -> {"f", float, Precision};
            "F" -> {"f", float, Precision};        % No uppercase version
            "g" -> {"g", float, Precision};
            "G" -> {"g", float, Precision};        % No uppercase version
            "c" -> {"tc", char, Precision};        % Assume unicode chars
            "r" -> {"p", any, Precision}; 
            "s" -> {"ts", string, Precision};      % Assume unicode strings
            "%" -> {"i%", any, Precision}
        end,
    FS1 = FS#fmt_spec{type=EFmtType, 
                      expected_arg=ExpectedArg,
                      precision=Precision1}, 
    parse_py_fmt_spec(RestFields, RestMatches, FS1);
parse_py_fmt_spec([rest], [Rest], FS) ->
    {ok, FS, Rest}.

py_fmt_spec_regex() ->
    MapKey = [mapkey, "((?:\\(.*?\\))?)"],
    Flags = [flags, "([#0 +-]*)"],
    MinFieldWidth = [min_field_width, "((?:\\d+|\\*)?)"],
    Precision = [precision, "((?:\\.(?:\\d+|\\*))?)"],
    LenModifier = "[hLl]?",
    ConversionType = [type, "([diouxXeEfFgGcrs%])"],
    Rest = [rest, "(.*)$"],
    L = lists:flatten(["^%", MapKey, Flags, MinFieldWidth, 
                       Precision, LenModifier, ConversionType, Rest]),
    IsFieldName = fun(T) -> is_atom(T) end,
    lists:partition(IsFieldName, L).

handle_fmt_flags([$# | _], _FS) ->
    {failed, python_alternate_form_not_supported};
handle_fmt_flags([Flag | Rest], FS) ->
    FS1 =
        case Flag of
            $0 ->
                FS#fmt_spec{pad_char=$0};
            $- ->
                FS#fmt_spec{justify=left};
            $\s ->
                FS;                         % Ignore Space Flag
            $+ ->
                FS                          % Ignore Plus Flag
        end,
    handle_fmt_flags(Rest, FS1);
handle_fmt_flags([], FS) ->
    {ok, FS}.

%%------------------------------------------------------------------------
%% Other
%%------------------------------------------------------------------------

trim("") ->
    "";
trim(String) ->
    re:replace(String, "^\\s*(.*?)\\s*$", "\\1", [unicode, {return, list}]).

normalize_absolute_path("/" ++ _ = FilePath) ->
    Parts = filename:split(FilePath),
    normalize_absolute_path(Parts, []).

normalize_absolute_path([".." | Rest], Acc) ->
    case Acc of
        ["/"] ->
            normalize_absolute_path(Rest, Acc);
        [_ | Acc1] ->
            normalize_absolute_path(Rest, Acc1)
    end;
normalize_absolute_path([Part | Rest], Acc) ->
    normalize_absolute_path(Rest, [Part | Acc]);
normalize_absolute_path([], Acc) ->
    filename:join(lists:reverse(Acc)).

normalize_relative_path(FilePath) ->
    case filename:split(FilePath) of
        ["/" | Parts] ->
            normalize_relative_path(Parts, []);
        Parts ->
            normalize_relative_path(Parts, [])
    end.

normalize_relative_path([".." | Rest], Acc) ->
    case Acc of
        [] ->
            illegal;
        [_ | Acc1] ->
            normalize_relative_path(Rest, Acc1)
    end;
normalize_relative_path([Part | Rest], Acc) ->
    normalize_relative_path(Rest, [Part | Acc]);
normalize_relative_path([], Acc) ->
    case Acc of
        [] -> 
            "";
        _ ->
            filename:join(lists:reverse(Acc))
    end.

hash(Term) ->
    erlang:phash2(Term, 16#FFFFFFFF).

