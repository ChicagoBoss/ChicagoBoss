%%%-------------------------------------------------------------------
%%% File    : etcher_std_filters.erl
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

-module(etcher_std_filters).
-export([standard_filters/0,
         filter_add/3,
         filter_addslashes/2,
         filter_capfirst/2,
         filter_center/3,
         filter_cut/3,
         filter_date/2,
         filter_date/3,
         filter_default/3,
         filter_default_if_none/3,
         filter_dictsort/3,
         filter_dictsortreversed/3,
         filter_divisibleby/3,
         filter_escape/2,
         filter_escapejs/2,
         filter_estringformat/3,
         filter_filesizeformat/2,
         filter_first/2,
         filter_fix_ampersands/2,
         filter_floatformat/2,
         filter_floatformat/3,
         filter_force_escape/2,
         filter_get_digit/3,
         filter_iriencode/2,
         filter_join/3,
         filter_last/2,
         filter_length/2,
         filter_length_is/3,
         filter_linebreaks/2,
         filter_linebreaksbr/2,
         filter_linenumbers/2,
         filter_ljust/3,
         filter_lower/2,
         filter_make_list/2,
         filter_phone2numeric/2,
         filter_pluralize/2,
         filter_pluralize/3,
         filter_pprint/2,
         filter_random/2,
         filter_removetags/3,
         filter_rjust/3,
         filter_safe/2,
         filter_safeseq/2,
         filter_sfirst/2,
         filter_slast/2,
         filter_slice/3,
         filter_slugify/2,
         filter_srandom/2,
         filter_stringformat/3,
         filter_striptags/2,
         filter_time/2,
         filter_time/3,
         filter_timesince/2,
         filter_timesince/3,
         filter_timeuntil/2,
         filter_timeuntil/3,
         filter_title/2,
         filter_truncatewords/3,
         filter_truncatewords_html/3,
         filter_udate/2,
         filter_udate/3,
         filter_unordered_list/2,
         filter_upper/2,
         filter_urlencode/2,
         filter_urlize/2,
         filter_urlizetrunc/3,
         filter_wordcount/2,
         filter_wordwrap/3,
         filter_yesno/2,
         filter_yesno/3
         ]).

-include("internal.hrl").

-define(UNICODE_LINE_SEP, 16#2028).
-define(UNICODE_PAR_SEP, 16#2029).

% Regex taken from django.util.html
-define(UNENCODED_AMPERSANDS_REGEX, "&(?!(\\w+|#\\d+);)").
-define(XML_TAG_REGEX, "<[^>]*?>").

% Regex taken from django.util.html
-define(URLIZE_LEADING_PUNCT, "((?:\\(|<|&lt;)*)").
-define(URLIZE_TRAILING_PUNCT, "((?:\\.|,|\\)|>|\n|&gt;)*)").
-define(URLIZE_PUNCTUATION_REGEX, 
            "^" ?URLIZE_LEADING_PUNCT "(.*?)" ?URLIZE_TRAILING_PUNCT "$").
-define(SIMPLE_EMAIL_RE, "^\\S+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9._-]+$").

% Taken from django.util.encoding.iri_to_uri()
-define(SAFE_IRI_CHARS, "/#%[]=:;$&()+,!?*").

% From: django.utils.text.truncate_html_words()
-define(HTML4_SINGLETS, ["br", "col", "link", "base", "img", 
                         "param", "area", "hr", "input"]).

-define(TABSIZE, 8).

-define(KILOBYTE, 1024).
-define(MEGABYTE, (1024 * ?KILOBYTE)).
-define(GIGABYTE, (1024 * ?MEGABYTE)).

-define(MIN_SECS, 60).
-define(HOUR_SECS, (60 * ?MIN_SECS)).
-define(DAY_SECS, (24 * ?HOUR_SECS)).
-define(WEEK_SECS, (7 * ?DAY_SECS)).
-define(MONTH_SECS, (30 * ?DAY_SECS)).
-define(YEAR_SECS, (365 * ?DAY_SECS)).

-define(DEFAULT_DATE_FORMAT, "N j, Y").
-define(DEFAULT_TIME_FORMAT, "P").

standard_filters() ->
    Filters = exported_filters(),
    to_filter_defs(Filters, []).

exported_filters() ->
    L = [{atom_to_list(F), Arity} || {exports, Exports} <- module_info(),
                                             {F, Arity} <- Exports,
                                         ((Arity =:= 2) or (Arity =:= 3))],
    [{Name, list_to_atom(F), Arity} || {"filter_" ++ Name = F, Arity} <- L].    

to_filter_defs([{Name, Function, Arity} | Rest], Acc) ->
    case proplists:delete(Name, Rest) of
        Rest when Arity =:= 2 ->
            Def = new_filter_def(Name, Function, false),
            to_filter_defs(Rest, [Def | Acc]);
        Rest when Arity =:= 3 ->
            Def = new_filter_def(Name, Function, true),
            to_filter_defs(Rest, [Def | Acc]);
        Rest1 ->
            Def = new_filter_def(Name, Function, optional),
            to_filter_defs(Rest1, [Def | Acc])
    end;
to_filter_defs([], Acc) ->
    lists:reverse(Acc).

new_filter_def(Name, Function, ArgRequired) ->
    #filter_def{
        name = Name,
        mf = {?MODULE, Function},
        arg_required = ArgRequired}.

%%------------------------------------------------------------------------
%% Strings
%%------------------------------------------------------------------------

filter_addslashes(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = addslashes(S, []),
    OrigStr#string{val=S1}.

addslashes([C | Rest], Acc) when ((C =:= $') or (C =:= $") or (C =:= $\\)) ->
    addslashes(Rest, [C, $\\ | Acc]);
addslashes([C | Rest], Acc) ->
    addslashes(Rest, [C | Acc]);
addslashes([], Acc) ->
    lists:reverse(Acc).

filter_capfirst(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = 
        case S of
            [C | Rest] ->
                [string:to_upper(C) | Rest];
            "" -> 
                ""
        end,
    OrigStr#string{val=S1}.

filter_escapejs(Val, _AutoEsc) ->
    #string{val=S} = to_string(Val),
    S1 = escape_js(S, []),
    #string{val=S1}.

escape_js([C | Rest], Acc) when ((C < 32) or
                                 (C =:= $\\) or (C =:= $') or (C =:= $") or
                                 (C =:= $>) or (C =:= $<) or (C =:= $&) or
                                 (C =:= $=) or (C =:= $-) or (C =:= $;)) ->
    S = "\\x" ++ erlang:integer_to_list(C, 16),
    escape_js(Rest, [S | Acc]);
escape_js([C | Rest], Acc) when ((C =:= ?UNICODE_LINE_SEP) or 
                                 (C =:= ?UNICODE_PAR_SEP)) ->
    S = "\\u" ++ erlang:integer_to_list(C, 16),
    escape_js(Rest, [S | Acc]);
escape_js([C | Rest], Acc) ->
    escape_js(Rest, [C | Acc]);
escape_js([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

filter_fix_ampersands(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = string_replace(S, ?UNENCODED_AMPERSANDS_REGEX, "&amp;"),
    OrigStr#string{val=S1}.

filter_floatformat(Val, AutoEsc) ->
    filter_floatformat(Val, AutoEsc, -1).

filter_floatformat(Val, AutoEsc, N) when not is_float(Val) ->
    try to_float(Val) of
        Float ->
            filter_floatformat(Float, AutoEsc, N)
    catch 
        error:badarg ->
            ?EMPTY_STRING
    end;
filter_floatformat(Float, AutoEsc, T) when not is_integer(T) ->
    try to_integer(T) of
        NumPlaces ->
            filter_floatformat(Float, AutoEsc, NumPlaces)
    catch
        error:badarg ->
            Float
    end;
filter_floatformat(Float, _AutoEsc, NumPlaces) ->
    FloatStr =
        case floatformat(Float, abs(NumPlaces)) of
            S when NumPlaces < 0 ->
                string_replace(S, "\\.0+$", "");
            S ->
                S
        end,
    #string{val=FloatStr, safe=true}.

floatformat(Float, NumPlaces) ->
    Fmt = "~." ++ integer_to_list(NumPlaces) ++ "f",
    S = io_lib:format(Fmt, [Float]),
    lists:flatten(S).

filter_iriencode(undefined, _AutoEsc) ->
    to_string(undefined);           % Mimicking Djangos implemenatation
filter_iriencode(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = etcher_util:url_quote(S, ?SAFE_IRI_CHARS),
    OrigStr#string{val=S1}.

% Not sure why Django autoescapes and marks the output of linenumbers as 
% safe, but that's what it does.
filter_linenumbers(Val, AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    Lines = split(S, $\n),
    Width = length(integer_to_list(length(Lines))),
    WidthStr = integer_to_list(Width),
    Lines1 = linenumbers(Lines, 1, WidthStr, []),
    S1 = string:join(Lines1, "\n"),
    ResultStr = AutoEsc(OrigStr#string{val=S1}),
    ResultStr#string{safe=true}.

linenumbers([Line | Rest], LineNum, Width, Acc) ->
    Fmt = "~" ++ Width ++ "..0b. ~ts",          % Use "~ts" for unicode strings
    Line1 = io_lib:format(Fmt, [LineNum, Line]),
    Line2 = lists:flatten(Line1),
    linenumbers(Rest, LineNum + 1, Width, [Line2 | Acc]);
linenumbers([], _LineNum, _Width, Acc) ->
    lists:reverse(Acc).

filter_lower(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = string:to_lower(S),
    OrigStr#string{val=S1}.

filter_make_list(#string{val=List}, _AutoEsc) ->
    List;
filter_make_list(Val, AutoEsc) ->
    filter_make_list(to_string(Val), AutoEsc).

filter_slugify(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = slugify(S, []),
    OrigStr#string{val=S1}.

slugify([C | Rest], Acc) when ?IS_LOWCASE_LETTER(C) or ?IS_DIGIT(C) ->
    slugify(Rest, [C | Acc]);
slugify([C | Rest], Acc) when ?IS_UPCASE_LETTER(C) ->
    slugify(Rest, [string:to_lower(C) | Acc]);
slugify([C | Rest], Acc) when ((C =:= $-) or (C =:= $\s))->
    Acc1 = 
        case Acc of
            [$- | _] ->
                Acc;
            _ ->
                [$- | Acc]
        end,
    slugify(Rest, Acc1);
slugify([_ | Rest], Acc) ->
    slugify(Rest, Acc);
slugify([], Acc) ->
    lists:reverse(Acc).

% Note: Django marks this 'is_safe' but we don't treat it as safe.
filter_stringformat(Val, AutoEsc, FmtArg) when is_binary(FmtArg); 
                                               ?IS_STRING(FmtArg) ->
    filter_stringformat(Val, AutoEsc, to_string(FmtArg));
filter_stringformat(Val, _AutoEsc, #string{val=PyFmtStr}) ->
    PyFmtStr1 = [$% | PyFmtStr],
    case etcher_util:py_to_erl_fmt_string(PyFmtStr1) of
        {ok, _ErlFmtStr, ExpectedArgs} when length(ExpectedArgs) > 1 ->
            ?EMPTY_STRING;
        {ok, ErlFmtStr, [ExpectedArg]} ->
            stringformat(Val, ExpectedArg, ErlFmtStr);
        {failed, _Reason} ->
            ?EMPTY_STRING
    end;
filter_stringformat(_Val, _AutoEsc, _NotStringArg) ->
    ?EMPTY_STRING.

stringformat(Val, int, ErlFmtStr) ->
    stringformat(to_integer(Val), ErlFmtStr);
stringformat(Val, float, ErlFmtStr) ->
    stringformat(to_float(Val), ErlFmtStr);
stringformat(Val, char, ErlFmtStr) when is_integer(Val) ->
    stringformat(Val, ErlFmtStr);
stringformat(Val, char, ErlFmtStr) when is_float(Val) ->
    % Python has deprecated this 
    stringformat(to_integer(Val), ErlFmtStr);
stringformat(Val, char, ErlFmtStr) ->
    case to_string(Val) of
        #string{val=[C]} ->
            stringformat(C, ErlFmtStr);
        _ ->
            ?EMPTY_STRING
    end;
stringformat(Val, string, ErlFmtStr) ->
    #string{val=S} = to_string(Val),
    stringformat(S, ErlFmtStr);
stringformat(Val, any, ErlFmtStr) ->
    stringformat(Val, ErlFmtStr).

stringformat(Val, ErlFmtStr) ->
    S = io_lib:format(ErlFmtStr, [Val]),
    S1 = lists:flatten(S),
    #string{val=S1}.

% NON-STANDARD FILTER
filter_estringformat(Val, AutoEsc, FmtArg) when is_binary(FmtArg);
                                                ?IS_STRING(FmtArg) ->
    filter_estringformat(Val, AutoEsc, to_string(FmtArg));
filter_estringformat(#string{val=S}, AutoEsc, FmtArg) ->
    filter_estringformat(S, AutoEsc, FmtArg);
filter_estringformat(Val, _AutoEsc, #string{val=FmtStr}) ->
    FmtStr1 = [$~ | FmtStr],
    try 
        S = io_lib:format(FmtStr1, [Val]),
        S1 = lists:flatten(S),
        #string{val=S1}
    catch
        error:badarg ->
            ?EMPTY_STRING
    end;
filter_estringformat(_Val, _AutoEsc, _NotStringArg) ->
    ?EMPTY_STRING.

filter_title(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = to_title_case(S, []),
    OrigStr#string{val=S1}.

to_title_case([C | Rest], Acc) ->
    C1 = 
        case Acc of
            [Letter | _] when ?IS_LETTER(Letter) ->
                string:to_lower(C);
            _ ->
                string:to_upper(C)
        end,
    to_title_case(Rest, [C1 | Acc]);
to_title_case([], Acc) ->
    lists:reverse(Acc).

filter_truncatewords(Val, _AutoEsc, Max) ->
    #string{val=S} = OrigStr = to_string(Val),
    Words = string_split(S),
    S1 =
        case to_integer(Max) of
            0 ->
                "";                             % Django would throw exception
            N when N < 0, abs(N) >= length(Words) ->
                "";                             % Django would throw exception
            N when N < 0 ->
                MaxWords = length(Words) - abs(N),
                truncatewords(Words, MaxWords);
            MaxWords ->
                truncatewords(Words, MaxWords)
        end,
    OrigStr#string{val=S1}.
            
truncatewords(Words, MaxWords) when length(Words) =< MaxWords ->
    string:join(Words, " ");
truncatewords(Words, MaxWords) ->
    {Words1, _} = lists:split(MaxWords, Words),
    LastWord = lists:last(Words1),
    Words2 = 
        case lists:suffix("...", LastWord) of
            true ->
                Words1;
            _ ->
                Words1 ++ ["..."]
        end,
    string:join(Words2, " ").

filter_truncatewords_html(Val, _AutoEsc, MaxWordsArg) ->
    #string{val=S} = OrigStr = to_string(Val),
    MaxWords = to_integer(MaxWordsArg),
    S1 = truncatewords_html(S, MaxWords),
    OrigStr#string{val=S1}.

% COPYRIGHT: Logic, including most regular expressions, taken 
%            from django.utils.text.truncate_html_words()
truncatewords_html(S, MaxWords) ->
    EnityRe = "&.*?;",
    TagLikeRe = "<.*?>",
    WordRe = "\\w[\\w-]*",
    Regex = ["(", EnityRe, "|", TagLikeRe, "|", WordRe, ")"],
    Chunks = string_split(S, Regex),
    Tokens = trunc_html_tokenize(Chunks, []),
    case trunc_html_count_words(Tokens) of
        TotalWords when TotalWords > MaxWords ->
            trunc_html(Tokens, MaxWords, [], []);
        _ ->
            S
    end.
   
trunc_html_tokenize(["<" ++ _ = S | Rest], Acc) ->
    LowCaseS = string:to_lower(S),          % Capture lower case tag names
    % Unfortunatly re:compile is (was when writing this) buggy. It ignores
    % the unicode flag.
    TagRegex = "^<(/?)([^ /]+).*?(/?)>$",
    Tok =
        case string_match(LowCaseS, TagRegex) of
            {match, ["/", _, "/"]} ->       % Corrupt Tag
                {text, S};
            {match, ["/", TagName, _]} ->
                {end_tag, TagName, S};
            {match, [_, TagName, "/"]} ->
                {full_tag, TagName, S};
            {match, [_, "!" ++ _ , _]} ->     % Comments, DocTypes, etc
                {text, S};
            {match, [_, TagName, _]} ->
                {start_tag, TagName, S};
            nomatch ->
                {text, S}
        end,
    trunc_html_tokenize(Rest, [Tok | Acc]);
trunc_html_tokenize([[C | _] = Word | Rest], Acc) when ?IS_ALPHA_NUM(C) ->
    Tok = {word, Word},
    trunc_html_tokenize(Rest, [Tok | Acc]);
trunc_html_tokenize([Other | Rest], Acc) ->
    Tok = {text, Other},
    trunc_html_tokenize(Rest, [Tok | Acc]);
trunc_html_tokenize([], Acc) ->
    lists:reverse(Acc).

trunc_html_count_words(Tokens) ->
    F = fun({word, _}, Count) ->
                Count + 1;
           (_, Count) ->
                Count
        end,
    lists:foldl(F, 0, Tokens).

trunc_html(_Tokens, Vacancies, OpenTags, Acc) when Vacancies < 1 ->
    trunc_html_close_tags(OpenTags, Acc);    
trunc_html([{word, Word} | Rest], Vacancies, OpenTags, Acc) ->
    case Vacancies - 1 of
        0 ->
            trunc_html(Rest, 0, OpenTags, [" ...", Word | Acc]);
        Vacancies1 ->
            trunc_html(Rest, Vacancies1, OpenTags, [Word | Acc])
    end;
trunc_html([{full_tag, _TagName, Raw} | Rest], Vacancies, OpenTags, Acc) ->
    trunc_html(Rest, Vacancies, OpenTags, [Raw | Acc]);
trunc_html([{start_tag, TagName, Raw} | Rest], Vacancies, OpenTags, Acc) ->
    Html4Singlets = ["meta", "link" | ?HTML4_SINGLETS],
    case lists:member(TagName, Html4Singlets) of
        true ->
            trunc_html(Rest, Vacancies, OpenTags, [Raw | Acc]);
        false ->
            trunc_html(Rest, Vacancies, [TagName | OpenTags], [Raw | Acc])
    end;
trunc_html([{end_tag, TagName, Raw} | Rest], Vacancies, OpenTags, Acc) ->
    OpenTags1 = lists:delete(TagName, OpenTags),
    trunc_html(Rest, Vacancies, OpenTags1, [Raw | Acc]);
trunc_html([{text, Text} | Rest], Vacancies, OpenTags, Acc) ->
    trunc_html(Rest, Vacancies, OpenTags, [Text | Acc]).

trunc_html_close_tags([TagName | Rest], Acc) ->
    Tag = ["</", TagName, ">"],
    trunc_html_close_tags(Rest, [Tag | Acc]);
trunc_html_close_tags([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

filter_upper(Val, _AutoEsc) ->
    #string{val=S} = to_string(Val),
    S1 = string:to_upper(S),
    #string{val=S1}.

filter_urlencode(Val, _AutoEsc) ->
    #string{val=S} = to_string(Val),
    S1 = etcher_util:url_quote(S),
    #string{val=S1}.

filter_urlize(Val, AutoEsc) ->
    #string{val=S} = to_string(Val),
    S1 = urlize(S, AutoEsc),
    #string{val=S1, safe=true}.

% COPYRIGHT: urlize is translated from django.utils.html.urlize()
%            The layout here is different but all the logic is 
%            taken from the django implemenation.
urlize(S, AutoEsc) ->
    urlize(S, AutoEsc, length(S)).

urlize(S, AutoEsc, TrimLen) ->
    Words = string_split(S, "(\\s+)"),
    urlize(Words, AutoEsc, TrimLen, []).

urlize([Word | Rest], AutoEsc, TrimLen, Acc) ->
    Word1 = 
        case [C || C <- Word, (C =:= $.) or (C =:= $@) or (C =:= $:)] of
            [] ->
                AutoEsc(to_string(Word));
            _ ->
                urlize_word(Word, AutoEsc, TrimLen)
        end,
    urlize(Rest, AutoEsc, TrimLen, [Word1 | Acc]);
urlize([], _AutoEsc, _TrimLen, Acc) ->
    [C || #string{val=S} <- lists:reverse(Acc), C <- S].

urlize_word(Word, AutoEsc, TrimLen) ->
    case string_match(Word, ?URLIZE_PUNCTUATION_REGEX) of
        {match, [Lead, Middle, Trail]} ->
            urlize_scan_url(Middle, Lead, Trail, AutoEsc, TrimLen);
        nomatch ->
            AutoEsc(to_string(Word))
    end.

urlize_scan_url(Middle, Lead, Trail, AutoEsc, TrimLen) ->
    case urlize_scan_url(Middle) of
        {url, Url} ->
            AnchorText = urlize_trim_anchor_text(Middle, TrimLen),
            Parts = [Url, AnchorText, Lead, Trail],
            urlize_href(Parts, AutoEsc);
        none ->
            S = to_string(lists:flatten([Lead, Middle, Trail])),
            AutoEsc(S)
    end.

urlize_scan_url("http://" ++ _ = Url) ->
    urlize_quote_url(Url);
urlize_scan_url("https://" ++ _ = Url) ->
    urlize_quote_url(Url);
urlize_scan_url("www." ++ _ = Middle) ->
    urlize_quote_url("http://" ++ Middle);
urlize_scan_url([C | _] = Middle) ->
    case {lists:member($@, Middle), urlize_recognize_dot_ending(Middle)} of
        {true, _} ->
            urlize_scan_email(Middle);
        {false, true} when ?IS_ALPHA_NUM(C) ->
            urlize_quote_url("http://" ++ Middle);
        _ ->
            none
    end;
urlize_scan_url([]) ->
    none.
            
urlize_scan_email(Middle) ->
    case {lists:member($:, Middle), string_match(Middle, ?SIMPLE_EMAIL_RE)} of
        {false, {match, _}} ->
            {url, "mailto:" ++ Middle};
        _ ->
            none
    end.

urlize_recognize_dot_ending(S) ->
    EndsWith = fun(Ending) -> lists:suffix(Ending, S) end,
    lists:any(EndsWith, [".com", ".org", ".net"]).

urlize_quote_url(Url) ->
    SafeChars = "/&=:;#?+*",
    Url1 = etcher_util:url_quote(Url, SafeChars),
    {url, Url1}.

urlize_trim_anchor_text(_, TrimLen) when TrimLen =< 3 ->
    "...";
urlize_trim_anchor_text(S, TrimLen) when TrimLen >= length(S) ->
    S;
urlize_trim_anchor_text(S, TrimLen) ->
    string:substr(S, 1, TrimLen - 3) ++ "...".

urlize_href(Parts, AutoEsc) ->
    Parts1 = 
        [begin
            #string{val=S} = AutoEsc(to_string(Part)),
            S
        end || Part <- Parts],
    urlize_href(Parts1).

urlize_href([Url, AnchorText, Lead, Trail]) ->
    NoFollow = 
        case Url of
            "http" ++ _ ->
                " rel=\"nofollow\"";
            "mailto" ++ _ ->
                ""
        end,
    L = [Lead, 
         "<a href=\"", Url, "\"", NoFollow, ">", AnchorText, "</a>", 
         Trail],
    to_string(lists:flatten(L)).

filter_urlizetrunc(Val, AutoEsc, LenArg) ->
    #string{val=S} = to_string(Val),
    TrimLen = to_integer(LenArg),
    S1 = urlize(S, AutoEsc, TrimLen),
    #string{val=S1, safe=true}.

filter_wordcount(Val, _AutoEsc) ->
    #string{val=S} = to_string(Val),
    case trim(S) of
        "" ->
            0;
        S1 ->
            length(string_split(S1))
    end.

filter_wordwrap(Val, _AutoEsc, Arg) ->
    #string{val=S} = OrigStr = to_string(Val),
    MaxLen = to_integer(Arg),
    Tokens = wordwrap_tokenize(S),
    S1 = wordwrap(Tokens, 0, MaxLen, []),
    OrigStr#string{val=S1}.

wordwrap_tokenize(S) ->
    Regex = "((?:[\s\t]+)|(?:\r?\n))",
    L = string_split(S, Regex),
    wordwrap_tokenize(L, []).

wordwrap_tokenize([NL | Rest], Acc) when ((NL =:= "\n") or (NL =:= "\r\n")) ->
    wordwrap_tokenize(Rest, [{nl, NL} | Acc]);
wordwrap_tokenize([[C | _] = Space | Rest], Acc) when (C =:= $\s) or 
                                                      (C =:= $\t) ->
    IsTab = fun(Char) -> Char =:= $\t end,
    {Tabs, Spaces} = lists:partition(IsTab, Space),
    Length = (length(Tabs) * ?TABSIZE) + length(Spaces),
    Tok = {space, Length, Space},
    wordwrap_tokenize(Rest, [Tok | Acc]);
wordwrap_tokenize([Word | Rest], Acc) ->
    Tok = {word, length(Word), Word},
    wordwrap_tokenize(Rest, [Tok | Acc]);
wordwrap_tokenize([], Acc) ->
    lists:reverse(Acc).

wordwrap([{nl, _} = Tok | Rest], _LineLen, MaxLen, Acc) ->
    wordwrap(Rest, 0, MaxLen, [Tok | Acc]);
wordwrap([{TokType, TokLen, _} = Tok | Rest], LineLen, MaxLen, Acc) 
                                        when (TokLen + LineLen) > MaxLen ->
    case TokType of
        space ->
            wordwrap(Rest, 0, MaxLen, [{nl, "\n"} | Acc]);
        word ->
            wordwrap(Rest, TokLen, MaxLen, [Tok, {nl, "\n"} | Acc])
    end;
wordwrap([{_, TokLen, _} = Tok | Rest], LineLen, MaxLen, Acc) ->
    LineLen1 = LineLen + TokLen,
    wordwrap(Rest, LineLen1, MaxLen, [Tok | Acc]);
wordwrap([], _LineLen, _MaxLen, Acc) ->
    Tokens = lists:reverse(Acc),
    wordwrap_to_list(Tokens, []).

wordwrap_to_list([{space, _, _}, {nl, NL} | Rest], Acc) ->
    wordwrap_to_list(Rest, [NL | Acc]);
wordwrap_to_list([{nl, NL} | Rest], Acc) ->
    wordwrap_to_list(Rest, [NL | Acc]);
wordwrap_to_list([{_, _, Content} | Rest], Acc) ->
    wordwrap_to_list(Rest, [Content | Acc]);
wordwrap_to_list([], Acc) ->
    L = lists:reverse(Acc),
    lists:flatten(L).

filter_ljust(Val, _AutoEsc, FieldSizeArg) ->
    #string{val=S} = OrigStr = to_string(Val),
    FieldSize = to_integer(FieldSizeArg),
    S1 = 
        case FieldSize =< length(S) of
            true -> 
                S;
            false ->
                string:left(S, FieldSize)
        end,
    OrigStr#string{val=S1}.

filter_rjust(Val, _AutoEsc, FieldSizeArg) ->
    #string{val=S} = OrigStr = to_string(Val),
    FieldSize = to_integer(FieldSizeArg),
    S1 = 
        case FieldSize =< length(S) of
            true -> 
                S;
            false ->
                string:right(S, FieldSize)
        end,
    OrigStr#string{val=S1}.

filter_center(Val, _AutoEsc, FieldSizeArg) ->
    #string{val=S} = OrigStr = to_string(Val),
    FieldSize = to_integer(FieldSizeArg),
    S1 = 
        case FieldSize =< length(S) of
            true -> 
                S;
            false ->
                string:centre(S, FieldSize)
        end,
    OrigStr#string{val=S1}.

filter_cut(Val, _AutoEsc, CutThis) ->
    #string{val=S} = to_string(Val),
    #string{val=CutStr} = to_string(CutThis),
    case CutStr of
        "" ->
            #string{val=S};
        _ ->
            S1 = cut(S, CutStr, []),
            #string{val=S1}
    end.

cut([C | Rest] = S, CutStr, Acc) ->
    case cut_prefix(S, CutStr) of
        {ok, Rest1} ->
            cut(Rest1, CutStr, Acc);
        failed ->
            cut(Rest, CutStr, [C | Acc])
    end;
cut([], _CutStr, Acc) ->
    lists:reverse(Acc).

cut_prefix(Rest, []) ->
    {ok, Rest};
cut_prefix([C | Rest], [C | RestCutStr]) ->
    cut_prefix(Rest, RestCutStr);
cut_prefix(_, _) ->
    failed.

%%------------------------------------------------------------------------
%% HTML Strings
%%------------------------------------------------------------------------

% NOTE: The 'escape' filter is only meant to be applied after all 
%       the other filters. For this reason it is moved to the end of 
%       the pipeline at parse time. See etcher_parser for details.
filter_escape(Val, _AutoEsc) ->
    String = to_string(Val),
    etcher_util:conditional_escape(String).

filter_force_escape(Val, _AutoEsc) ->
    String = to_string(Val),
    etcher_util:force_escape(String).

% COPYRIGHT: Translated from django.utils.html.linebreaks()
filter_linebreaks(Val, AutoEsc) ->
    String = to_string(Val),
    #string{val=S} = AutoEsc(String),
    S1 = normalize_newlines(S),
    Paras = string_split(S1, "\n{2,}"),
    L = [begin
             P1 = replace(P, $\n, "<br />"),
             lists:flatten(["<p>", P1, "</p>"])
         end || P <- Paras],
    S2 = string:join(L, "\n\n"),
    #string{val=S2, safe=true}.

filter_linebreaksbr(Val, AutoEsc) ->
    String = to_string(Val),
    #string{val=S} = AutoEsc(String),
    S1 = replace(S, $\n, "<br />"),
    #string{val=S1, safe=true}.

filter_safe(Val, _AutoEsc) ->
    OrigStr = to_string(Val),
    OrigStr#string{safe=true}.

filter_safeseq(Val, AutoEsc) ->
    [filter_safe(Item, AutoEsc) || Item <- Val].

% COPYRIGHT: Translated from django.template.defaultfilters.removetags()
filter_removetags(Val, _AutoEsc, TagArg) ->
    #string{val=ValStr} = OrigStr = to_string(Val),
    #string{val=TagStr} = to_string(TagArg),
    Tags = string_split(TagStr),
    TagNamesRegex = ["(", string:join(Tags, "|"), ")"],
    ValStr1 = remove_start_tags(ValStr, TagNamesRegex),
    ValStr2 = remove_end_tags(ValStr1, TagNamesRegex),
    OrigStr#string{val=ValStr2}.

remove_start_tags(S, TagNamesRegex) ->
    StartTagRegex = ["<", TagNamesRegex, "(/?>|(\\s+[^>]*>))"],
    string_replace(S, StartTagRegex, "").

remove_end_tags(S, TagNamesRegex) ->
    EndTagRegex = ["</", TagNamesRegex, ">"],
    string_replace(S, EndTagRegex, "").

filter_striptags(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = string_replace(S, ?XML_TAG_REGEX, ""), 
    OrigStr#string{val=S1}.

%%------------------------------------------------------------------------
%% Lists
%%------------------------------------------------------------------------

filter_dictsort(Val, _AutoEsc, SortKey) ->
    #string{val=Key} = to_string(SortKey),
    Sortable = [{proplists:get_value(Key, PL), PL} || PL <- Val],
    [PL || {_, PL} <- lists:keysort(1, Sortable)].

filter_dictsortreversed(Val, AutoEsc, SortKey) ->
    lists:reverse(filter_dictsort(Val, AutoEsc, SortKey)).

filter_first(#string{val=S}, AutoEsc) ->
    filter_first(S, AutoEsc);
filter_first([FirstVal | _], _AutoEsc) ->
    FirstVal;
filter_first([], _AutoEsc) ->
    ?EMPTY_STRING.

% NON-STANDARD FILTER
filter_sfirst(#string{val=S}, AutoEsc) ->
    filter_sfirst(S, AutoEsc);
filter_sfirst([FirstChar | _], _AutoEsc) when is_integer(FirstChar) ->
    [FirstChar];
filter_sfirst(_, _AutoEsc) ->
    ?EMPTY_STRING.

filter_join(Val, _AutoEsc, _JoinArg) when not is_list(Val) ->
    Val;
filter_join(Val, AutoEsc, JoinArg) when is_list(JoinArg) ->
    filter_join(Val, AutoEsc, to_string(JoinArg));
filter_join(Val, AutoEsc, #string{val=JoinStr}) ->
    L = [begin
            String = to_string(Item),
            #string{val=S} = AutoEsc(String),
            S
         end || Item <- Val],
    S = string:join(L, JoinStr),
    #string{val=S, safe=true};
filter_join(Val, _AutoEsc, _NotStrArg) ->
    Val.

filter_last(#string{val=S}, AutoEsc) ->
    filter_last(S, AutoEsc);
filter_last(Val, _AutoEsc) ->
    lists:last(Val).

% NON-STANDARD FILTER
filter_slast(#string{val=S}, AutoEsc) ->
    filter_slast(S, AutoEsc);
filter_slast(Val, _AutoEsc) ->
    case lists:last(Val) of
        Char when is_integer(Char) ->
            [Char];
        _ ->
            ?EMPTY_STRING
    end.

filter_length(#string{val=S}, AutoEsc) ->
    filter_length(S, AutoEsc);
filter_length(Val, _AutoEsc) when is_list(Val) ->
    length(Val);
filter_length(_Val, _AutoEsc) ->
    ?EMPTY_STRING.

filter_length_is(#string{val=S}, AutoEsc, Len) ->
    filter_length_is(S, AutoEsc, Len);
filter_length_is(Val, _AutoEsc, Len) when is_list(Val), is_integer(Len) ->
    length(Val) =:= Len;
filter_length_is(_Val, _AutoEsc, _Len) ->
    ?EMPTY_STRING.

filter_random(#string{val=S}, AutoEsc)  ->
    filter_random(S, AutoEsc);
filter_random(Val, _AutoEsc) when is_list(Val) ->
    Index = random:uniform(length(Val)),
    lists:nth(Index, Val);
filter_random(_Val, _AutoEsc) ->
    ?EMPTY_STRING.

% NON-STANDARD FILTER
filter_srandom(#string{val=S}, AutoEsc)  ->
    filter_srandom(S, AutoEsc);
filter_srandom(Val, _AutoEsc) when is_list(Val) ->
    Index = random:uniform(length(Val)),
    case lists:nth(Index, Val) of
        Char when is_integer(Char) ->
            [Char];
        _ ->
            ?EMPTY_STRING
    end;
filter_srandom(_Val, _AutoEsc) ->
    ?EMPTY_STRING.

filter_slice(#string{val=S}, AutoEsc, SliceArg) ->
    filter_slice(S, AutoEsc, SliceArg);
filter_slice(Val, AutoEsc, #string{val=SliceStr}) ->
    filter_slice(Val, AutoEsc, SliceStr);
filter_slice(Val, _AutoEsc, SliceStr) when is_list(Val), is_list(SliceStr) ->
    case parse_slice_str(SliceStr, length(Val)) of
        {StartIndex, EndIndex} when StartIndex >= EndIndex ->
            [];
        {StartIndex, EndIndex} ->
            SliceLen = EndIndex - StartIndex,
            % slice uses zero-based indexing so we need to compensate
            StartIndex1 = StartIndex + 1,
            lists:sublist(Val, StartIndex1, SliceLen);
        failed ->
            Val
    end.

parse_slice_str(SliceStr, ListLen) ->
    case split(SliceStr, $:) of
        [StartIndexStr, EndIndexStr] ->
            try parse_slice_str(StartIndexStr, EndIndexStr, ListLen)
            catch
                error:badarg ->
                    failed
            end;
        _ ->
            failed
    end.

parse_slice_str("", EndIndex, ListLen) ->
    parse_slice_str(0, EndIndex, ListLen);
parse_slice_str(StartIndex, "", ListLen) ->
    parse_slice_str(StartIndex, ListLen, ListLen);
parse_slice_str(S, EndIndex, ListLen) when is_list(S) ->
    StartIndex = parse_slice_index(S, ListLen),
    parse_slice_str(StartIndex, EndIndex, ListLen);
parse_slice_str(StartIndex, S, ListLen) when is_list(S) ->
    EndIndex = parse_slice_index(S, ListLen),
    parse_slice_str(StartIndex, EndIndex, ListLen);
parse_slice_str(StartIndex, EndIndex, _ListLen) ->
    {StartIndex, EndIndex}.

parse_slice_index(S, ListLen) ->
    case to_integer(S) of
        N when N < 0, abs(N) > ListLen ->
            0;
        N when N < 0 ->
            ListLen - abs(N);
        N when N > ListLen ->
            ListLen;
        N ->
            N
    end.

% For compatibility with Django, we allow the unordered_list filter
% to be applied to a string as well as a nested list of strings.
filter_unordered_list(#string{val=S}, AutoEsc) ->
    filter_unordered_list(S, AutoEsc);
filter_unordered_list(S, AutoEsc) when ?IS_STRING(S) ->
    % Map string to a list of char strings
    case [[C] || C <- S, is_integer(C)] of
        L when length(L) =:= length(S) ->
            filter_unordered_list(L, AutoEsc);
        _ ->
            % Input was a mixed list of some sort
            ?EMPTY_STRING
    end;
filter_unordered_list(Val, AutoEsc) when is_list(Val) ->
    InitalIndent = "\t",
    IoList = unordered_list(Val, InitalIndent, AutoEsc, []),
    case unicode:characters_to_list(IoList) of
        S when is_list(S) ->
            String = to_string(S),
            String#string{safe=true}
    end.

unordered_list([RawItem | Rest], Indent, AutoEsc, Acc) when 
                                ?IS_STRING(RawItem); is_binary(RawItem) ->
    #string{val=Item} = AutoEsc(to_string(RawItem)),
    {Content, Rest1} =  
        case Rest of
            [SubList | R] when is_list(SubList), not ?IS_STRING(SubList) ->
                T = [Item, "\n", ul_sublist(SubList, Indent, AutoEsc)],
                {T, R};
            _ ->
                {Item, Rest}
        end,
    Tag = li_tag(Content, Indent),
    unordered_list(Rest1, Indent, AutoEsc, [Tag | Acc]);
unordered_list([SubList | Rest], Indent, AutoEsc, Acc) when is_list(SubList) ->
    Content = ul_sublist(SubList, Indent, AutoEsc),
    Tag = li_tag(Content, Indent),
    unordered_list(Rest, Indent, AutoEsc, [Tag | Acc]);
unordered_list([], _Indent, _AutoEsc, Acc) ->
    lists:reverse(Acc).

ul_sublist(L, Indent, AutoEsc) ->
    BiggerIndent = add_tab(Indent),
    Content = unordered_list(L, BiggerIndent, AutoEsc, []), 
    [Indent, "<ul>", "\n", Content, Indent, "</ul>", "\n", Indent].

li_tag(Content, Indent) ->
    [Indent, "<li>", Content, "</li>", "\n"].

add_tab(S) ->
    [$\t | S].

%%------------------------------------------------------------------------
%% Integers
%%------------------------------------------------------------------------

filter_add(Val, _AutoEsc, Arg) ->
    to_integer(Val) + to_integer(Arg).

filter_get_digit(Val, _AutoEsc, PlaceArg) ->
    try 
        IntVal = to_integer(Val),
        NumStr = integer_to_list(IntVal),
        case to_integer(PlaceArg) of
            Place when Place < 1 ->
                IntVal;
            Place when Place > length(NumStr) ->
                0;
            Place ->
                C = lists:nth(Place, lists:reverse(NumStr)),
                C - $0
        end
    catch
        error:badarg ->
            Val
    end.

%%------------------------------------------------------------------------
%% Dates
%%------------------------------------------------------------------------

filter_date(Date, AutoEsc) ->
    filter_date(Date, AutoEsc, ?DEFAULT_DATE_FORMAT).

filter_date({_,_,_} = NowFmtDate, AutoEsc, FmtArg) ->
    LocalTime = calendar:now_to_local_time(NowFmtDate),
    filter_date(LocalTime, AutoEsc, FmtArg);
filter_date({{_,_,_}, {_,_,_}} = Date, _AutoEsc, FmtArg) ->
    % Assumes date is in Local Time
    #string{val=FmtStr} = to_string(FmtArg),
    S = etcher_dateformat:format_date(Date, FmtStr),
    #string{val=S};
filter_date(_NotDate, _AutoEsc, _FmtArg) ->
    "".

% NON-STANDARD FILTER
filter_udate(UtcDate, AutoEsc) ->
    filter_udate(UtcDate, AutoEsc, ?DEFAULT_DATE_FORMAT).

filter_udate({{_,_,_}, {_,_,_}} = UtcDate, AutoEsc, FmtArg) ->
    % Assumes date is in UTC
    LocalDate = calendar:universal_time_to_local_time(UtcDate),
    filter_date(LocalDate, AutoEsc, FmtArg);
filter_udate(_NotDate, _AutoEsc, _FmtArg) ->
    "".

filter_time(Time, AutoEsc) ->
    filter_time(Time, AutoEsc, ?DEFAULT_TIME_FORMAT).

% Unlike the 'date' filter, this doesn't accept a now() format 
% date as argument, because it has the same form, {_,_,_}, as 
% a time does.
filter_time({{_,_,_}, {_,_,_} = Time}, AutoEsc, FmtArg) ->
    filter_time(Time, AutoEsc, FmtArg);
filter_time({_,_,_} = Time, _AutoEsc, FmtArg) ->
    #string{val=FmtStr} = to_string(FmtArg),
    S = etcher_dateformat:format_time(Time, FmtStr),
    #string{val=S};
filter_time(_NotTimeOrDate, _AutoEsc, _FmtArg) ->
    "".

filter_timesince(Val, AutoEsc) ->
    Now = erlang:localtime(),    
    filter_timesince(Val, AutoEsc, Now).

filter_timesince(StartDate, _AutoEsc, EndDate) ->
    S = format_interval_between(StartDate, EndDate),
    #string{val=S}.

filter_timeuntil(Val, AutoEsc) ->
    Now = erlang:localtime(),    
    filter_timeuntil(Val, AutoEsc, Now).

filter_timeuntil(EndDate, _AutoEsc, StartDate) ->
    S = format_interval_between(StartDate, EndDate),
    #string{val=S}.

format_interval_between({_, _, _} = StartDate, EndDate) ->
    LocalStartDate = calendar:now_to_local_time(StartDate),
    format_interval_between(LocalStartDate, EndDate);
format_interval_between(StartDate, {_, _, _} = EndDate) ->
    LocalEndDate = calendar:now_to_local_time(EndDate),
    format_interval_between(StartDate, LocalEndDate);
format_interval_between({_, _} = StartDate, {_, _} = EndDate) ->
    StartGregSecs = calendar:datetime_to_gregorian_seconds(StartDate),
    EndGregSecs = calendar:datetime_to_gregorian_seconds(EndDate),
    DiffSecs = EndGregSecs - StartGregSecs,
    format_interval(DiffSecs).

format_interval(Secs) when Secs =< 0 ->
    "0 Minutes";
format_interval(Secs) ->
    Intervals = split_secs(Secs),
    Parts = select_intervals(Intervals),
    L = [lists:flatten(format_interval_part(P)) || P <- Parts],
    string:join(L, ", ").

select_intervals([Last]) ->
    [Last];
select_intervals([{N1, _} = Interval1, {N2, _} = Interval2 | _]) when N1 > 0 ->
    case N2 of
        0 ->
            [Interval1];
        _ ->
            [Interval1, Interval2]
    end;
select_intervals([_ | Rest]) ->
    select_intervals(Rest).

split_secs(Secs) ->
    UnitSpecs = [{?YEAR_SECS, "Year"},
                 {?MONTH_SECS, "Month"},
                 {?WEEK_SECS, "Week"},
                 {?DAY_SECS, "Day"},
                 {?HOUR_SECS, "Hour"},
                 {?MIN_SECS, "Minute"}],
    split_secs(UnitSpecs, Secs, []).

split_secs([{_, Unit} | Rest], 0, Acc) ->
    split_secs(Rest, 0, [{0, Unit} | Acc]);
split_secs([{UnitSz, Unit} | Rest], Secs, Acc) ->
    N = Secs div UnitSz,
    Secs1 = Secs rem UnitSz,
    split_secs(Rest, Secs1, [{N, Unit} | Acc]);
split_secs([], _Secs, Acc) ->
    lists:reverse(Acc).

format_interval_part({1, Unit}) -> 
    ["1 ", Unit];
format_interval_part({N, Unit}) -> 
    [integer_to_list(N), " ", Unit, "s"].

%%------------------------------------------------------------------------
%% Logic
%%------------------------------------------------------------------------

filter_default(Val, _AutoEsc, Alt) when ?EVALS_TO_FALSE(Val) ->
    Alt;
filter_default(Val, _AutoEsc, _Alt) ->
    Val.

filter_default_if_none(undefined, _AutoEsc, Alt) ->
    Alt;
filter_default_if_none(Val, _AutoEsc, _Alt) ->
    Val.

filter_divisibleby(Val, _AutoEsc, Arg) ->
    to_integer(Val) rem to_integer(Arg) =:= 0.

filter_yesno(Val, AutoEsc) ->
    filter_yesno(Val, AutoEsc, "yes,no,maybe").

filter_yesno(Val, AutoEsc, #string{val=ArgStr}) ->
    filter_yesno(Val, AutoEsc, ArgStr);
filter_yesno(Val, _AutoEsc, Arg) ->
    case split(Arg, $,) of
        [Y, N] ->
            yes_no_maybe(Val, {Y, N, N});
        [Y, N, M | _] ->
            yes_no_maybe(Val, {Y, N, M});
        _ ->
            Val
    end.

yes_no_maybe(undefined, {_, _, M}) ->
    M;
yes_no_maybe(Val, {_, N, _}) when ?EVALS_TO_FALSE(Val) ->
    N;
yes_no_maybe(_Val, {Y, _, _}) ->
    Y.

%%------------------------------------------------------------------------
%% Others
%%------------------------------------------------------------------------

filter_filesizeformat(Val, _AutoEsc) ->
    try to_float(Val) of
        Bytes when Bytes >= ?GIGABYTE ->
            filesizeformat(Bytes / ?GIGABYTE, "GB");
        Bytes when Bytes >= ?MEGABYTE ->
            filesizeformat(Bytes / ?MEGABYTE, "MB");
        Bytes when Bytes >= ?KILOBYTE ->
            filesizeformat(Bytes / ?KILOBYTE, "KB");
        Bytes ->
            S = integer_to_list(to_integer(Bytes)),
            S1 = S ++ " bytes",
            #string{val=S1, safe=true}
    catch
        error:badarg ->
            % Note: Django throws a runtime exception for this
            ""
    end.

filesizeformat(Float, UnitStr) ->
    S = io_lib:format("~.1f ~s", [Float, UnitStr]),
    S1 = lists:flatten(S),
    #string{val=S1, safe=true}.

filter_phone2numeric(Val, _AutoEsc) ->
    #string{val=S} = OrigStr = to_string(Val),
    S1 = phone2numeric(S, []),
    OrigStr#string{val=S1}.

phone2numeric([C | Rest], Acc) when ?IS_UPCASE_LETTER(C) ->
    phone2numeric([string:to_lower(C) | Rest], Acc);
phone2numeric([C | Rest], Acc) when ?IS_LOWCASE_LETTER(C) ->
    Tab = [{"abc", $2}, {"def", $3}, 
           {"ghi", $4}, {"jkl", $5},
           {"mno", $6}, {"pqrs", $7}, 
           {"tuv", $8}, {"wxyz", $9}],
    [Digit] = [Digit || {Letters, Digit} <- Tab, lists:member(C, Letters)],
    phone2numeric(Rest, [Digit | Acc]);
phone2numeric([C | Rest], Acc) ->
    phone2numeric(Rest, [C | Acc]);
phone2numeric([], Acc) ->
    lists:reverse(Acc).

filter_pluralize(Val, AutoEsc) ->
    filter_pluralize(Val, AutoEsc, #string{val="s"}).

filter_pluralize(#string{val=S}, AutoEsc, SuffixDef) ->
    filter_pluralize(S, AutoEsc, SuffixDef);
filter_pluralize(Val, _AutoEsc, #string{val=SuffixDef}) ->
    case split(SuffixDef, $,) of
        [Plur] ->
            pluralize(Val, "", Plur);
        [Sing, Plur] ->
            pluralize(Val, Sing, Plur);
        _ ->
            ?EMPTY_STRING
    end;
filter_pluralize(Val, AutoEsc, SuffixDef) ->
    filter_pluralize(Val, AutoEsc, to_string(SuffixDef)).

pluralize(Int, Sing, Plur) when is_integer(Int) ->
    case Int of   
        1 ->
            Sing;
        _ ->
            Plur
    end;
pluralize(L, Sing, Plur) when is_list(L), not ?IS_STRING(L) ->
    pluralize(length(L), Sing, Plur);
pluralize([], Sing, Plur) ->
    pluralize(0, Sing, Plur);
pluralize(T, Sing, Plur) ->
    try to_integer(T) of
        Int ->
            pluralize(Int, Sing, Plur)
    catch
        error:bararg ->
            Sing
    end.

filter_pprint(Val, _AutoEsc) ->
    etcher_util:to_ppstring(Val).

%%------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------

to_string(Val) ->
    etcher_util:to_string(Val).

to_integer(Val) ->
    etcher_util:to_integer(Val).

to_float(Val) ->
    etcher_util:to_float(Val).

string_replace(S, Regex, Replacement) ->
    re:replace(S, Regex, Replacement, 
                    [unicode, global, dotall, {return, list}]).

string_match(S, Regex) ->
    re:run(S, Regex, [unicode, dotall, {capture, all_but_first, list}]).

string_split(S) ->
    string_split(S, "\\s+").

string_split(S, Regex) ->
    etcher_util:split(S, Regex, [unicode, dotall, {return, list}]).

trim(S) ->
    etcher_util:trim(S).

% Different from string:tokens/2 - splits on every occurance of Char
split(S, C) when is_integer(C) ->
    split(S, C, [], []).

split([C | Rest], C, Acc, AccLines) ->
    AccLines1 = [lists:reverse(Acc) | AccLines],
    split(Rest, C, [], AccLines1);
split([I | Rest], C, Acc, AccLines) ->
    split(Rest, C, [I | Acc], AccLines);
split([], _C, Acc, AccLines) ->
    AccLines1 = [lists:reverse(Acc) | AccLines],
    lists:reverse(AccLines1).

replace(S, Char, Replacement) when is_integer(Char) ->
    replace(S, Char, Replacement, []).

replace([Char | Rest], Char, Replacement, Acc) ->
    case is_integer(Replacement) of
        true ->
            replace(Rest, Char, Replacement, [Replacement | Acc]);
        false ->
            Acc1 = lists:reverse(Replacement, Acc),
            replace(Rest, Char, Replacement, Acc1)
    end;
replace([C | Rest], Char, Replacement, Acc) ->
    replace(Rest, Char, Replacement, [C | Acc]);
replace([], _Char, _Replacement, Acc) ->
    lists:reverse(Acc).

normalize_newlines(S) ->
    normalize_newlines(S, []).

normalize_newlines([$\r, $\n | Rest], Acc) ->
    normalize_newlines(Rest, [$\n | Acc]);
normalize_newlines([$\r | Rest], Acc) ->
    normalize_newlines(Rest, [$\n | Acc]);
normalize_newlines([C | Rest], Acc) ->
    normalize_newlines(Rest, [C | Acc]);
normalize_newlines([], Acc) ->
    lists:reverse(Acc).

