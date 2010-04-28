%%%-------------------------------------------------------------------
%%% File    : etcher_scanner.erl
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

-module(etcher_scanner).
-export([scan/1,
         scan_var/1,
         scan_item/1,
         scan_string/1
         ]).

-include("internal.hrl").

-define(TAG_START, "{%").
-define(VAR_START, "{{").

-define(TAG_START_REGEX, "\\{%").
-define(TAG_END_REGEX, "%\\}").

-define(VAR_START_REGEX, "\\{\\{").
-define(VAR_END_REGEX, "\\}\\}").

-define(COMMENT_START_REGEX, "\\{#").
-define(COMMENT_END_REGEX, "#\\}").

-define(ESC_START_REGEX, "//--\\*").
-define(ESC_END_REGEX, "\\*--//").

-define(INITIAL_SPLIT, 
             "(" ?TAG_START_REGEX ".*?)" ?TAG_END_REGEX 
            "|(" ?VAR_START_REGEX ".*?)" ?VAR_END_REGEX
            "|" ?COMMENT_START_REGEX ".*?" ?COMMENT_END_REGEX
            "|" ?ESC_START_REGEX "(.*?)" ?ESC_END_REGEX
            ).

-define(FLOAT_REGEX, "[\\+-]?\\d+\\.\\d+(?:[Ee][\\+-]?\\d+)?").
-define(PY_FLOAT_REGEX, "[\\+-]?\\d+[Ee][\\+-]?\\d+").
-define(INTEGER_REGEX, "[\\+-]?\\d+").

-define(IS_NUM_SIGN(C), ((C =:= $+) or (C =:= $-))).
-define(IS_VAR_REF_CHAR(C), ((C =:= $.) or 
                             (C =:= $_) or
                             ((C >= $a) and (C =< $z)) or
                             ((C >= $A) and (C =< $Z)) or
                             ?IS_DIGIT(C))).

scan(Source) ->
    Chunks = macro_scan(Source),
    micro_scan(Chunks, []).

macro_scan(Source) ->
    split(Source, ?INITIAL_SPLIT, [unicode, dotall, {return, binary}]).

micro_scan([<<?TAG_START, B/bytes>> | Rest], Acc) ->
    Tag = scan_tag(unicode:characters_to_list(B)),
    micro_scan(Rest, [Tag | Acc]);
micro_scan([<<?VAR_START, B/bytes>> | Rest], Acc) ->
    Var = scan_var(unicode:characters_to_list(B)),
    micro_scan(Rest, [Var | Acc]);
micro_scan([B | Rest], Acc) ->
    %% Combine any sibling binaries. 
    case Acc of
        [LastBin | Acc1] when is_binary(LastBin) ->
            B1 = <<LastBin/bytes, B/bytes>>,
            micro_scan(Rest, [B1 | Acc1]);
        _ ->
            micro_scan(Rest, [B | Acc])
    end;
micro_scan([], Acc) ->
    {ok, lists:reverse(Acc)}.

%%------------------------------------------------------------------------
%% Scan Tag
%%------------------------------------------------------------------------

scan_tag(S) ->
    S1 = strip_edge_space(S),
    TagRegex = "^(\\w+)\\s*(.*)$",
    case match(S1, TagRegex) of
        {match, [Name, Extra]} ->
            #tag{name=Name, extra=Extra};
        nomatch ->
            throw({invalid_tag, S})
    end.

%%------------------------------------------------------------------------
%% Scan Variable 
%%------------------------------------------------------------------------

scan_var(S) ->
    S1 = strip_edge_space(S),
    {VarVal, Rest} = scan_var_val(S1),
    Filters = scan_filters(Rest, []),
    #variable{val=VarVal, filters=Filters}.

scan_var_val(S) ->
    case scan_item(S) of
        {ok, VarVal, Rest} ->
            {VarVal, Rest};
        none ->
            throw({invalid_variable_value, S})
    end.

scan_filters([], Acc) ->
    lists:reverse(Acc);
scan_filters([$| | Rest], Acc) ->
    case scan_ref(Rest) of
        {ok, #ref{parts=[Name]}, Rest1} when is_list(Name) ->
            {Arg, Rest2} = scan_filter_arg(Rest1),
            Filter = new_filter(Name, Arg),    
            scan_filters(Rest2, [Filter | Acc]);
        _ ->
            throw({invalid_filter, {bad_name, Rest}})
    end;
scan_filters(Rest, _Acc) ->
    throw({invalid_filter, {expected, '|'}, {rest, Rest}}).

scan_filter_arg([$: | Rest]) ->
    case scan_item(Rest) of
        {ok, Arg, Rest1} ->
            {Arg, Rest1};
        none ->
            throw({invalid_filter, {bad_arg, Rest}})
    end;
scan_filter_arg(Rest) ->
    {undefined, Rest}.

%%------------------------------------------------------------------------
%% Lower-level Scans
%%------------------------------------------------------------------------

%% The language is ambiguous with respect to floats, integers and 
%% variable references. From a lexical perspective, integers and floats
%% would qualify as legal variable references. Django decides at 
%% compile time whether a value is a literal or a variable reference. 
%% We do the same.
%%
%% If a float or an integer (without a preceeding sign) is followed 
%% by \w+ or a "." we treat it as a variable reference, otherwise
%% we treat it as a literal. Eg:
%%
%%   1.0, 33.46, 9.2e-18, 77e10  ==> Treat as floats
%%   6.2woof  ==> Treat as variable reference
%%   6.2.woof  ==> Treat as variable reference
%%   +6.2woof  ==> Treat as 6.2 followed by "woof" because of '+' sign
%%   6.2e10.woof  ==> Treat as variable reference
%%   6.2.8.4  ==> Treat as variable reference
%%   -6.2.8.4  ==> Treat as -6.2 followed by ".8.4" because of '-' sign
%%   44woof  ==> Treat as variable reference
%%   -44woof  ==> Treat as -44 followed by "woof" because of '-' sign
%%   44.woof  ==> Treat as variable reference
%%   +44.woof  ==> Treat as 44 followed by ".woof" because of '+' sign
%%
scan_item([C | _] = S) when ?IS_DIGIT(C) or ?IS_NUM_SIGN(C) ->
    case scan_float(S) of
        {ok, _Float, [C1 | _]} when ?IS_DIGIT(C) and ?IS_VAR_REF_CHAR(C1) ->
            scan_ref(S);
        {ok, _Float, _Rest} = T ->
            T;
        none ->
            case scan_integer(S) of
                {ok, _Integer, [C1 | _]} when ?IS_DIGIT(C) and 
                                              ?IS_VAR_REF_CHAR(C1) ->
                    scan_ref(S);
                {ok, _Integer, _Rest} = T ->
                    T;
                none ->
                    none
            end
    end;
scan_item([C | _] = S) when (C =:= $") or (C =:= $') ->
    case scan_string(S) of
        {ok, _Str, _Rest} = T ->
            T;
        none ->
            throw({missing_closing_quote, S})
    end;
scan_item(Rest) ->
    scan_ref(Rest).

scan_string([Delim | Rest]) ->
    scan_string(Rest, Delim, []).

% All literal strings are considered 'safe' - the template author should have
% escaped any XML/HTML characters that needed escaping.
scan_string([Delim | Rest], Delim, Acc) ->
    S = lists:reverse(Acc),
    SafeString = #string{val=S, safe=true},
    {ok, SafeString, Rest};
scan_string([$\\, C | Rest], Delim, Acc) ->
    scan_string(Rest, Delim, [C | Acc]); 
scan_string([C | Rest], Delim, Acc) ->
    scan_string(Rest, Delim, [C | Acc]); 
scan_string([], _Delim, _Acc) ->
    none.

scan_float(S) ->
    case scan_next(S, ?FLOAT_REGEX) of
        {match, [FloatStr, Rest]} ->
            Float = list_to_float(FloatStr),
            {ok, Float, Rest};
        nomatch ->
            scan_py_float(S)
    end.

%% Python allows float literals without a decimal part - eg 6e10
%% Erlang expects the decimal part, so we handle these specially.
%% Python also allows float literals which end in a decimal 
%% point (eg 10. 606.) but Django dissallows these, so we ignore them.
scan_py_float(S) ->
    case scan_next(S, ?PY_FLOAT_REGEX) of
        {match, [PyFloatStr, Rest]} ->
            FloatStr = replace(PyFloatStr, "[eE]", ".0e"),
            Float = list_to_float(FloatStr),
            {ok, Float, Rest};
        nomatch ->
            none
    end.

scan_integer(S) ->
    case scan_next(S, ?INTEGER_REGEX) of
        {match, [IntStr, Rest]} ->
            Integer = list_to_integer(IntStr),
            {ok, Integer, Rest};
        nomatch ->
            none
    end.

scan_ref(S) ->
    scan_ref(S, []).
    
scan_ref(S, Acc) ->
    case scan_next(S, "\\w+") of
        {match, [P, [$. | Rest]]} ->
            RefPart = to_ref_part(P),
            scan_ref(Rest, [RefPart | Acc]);
        {match, [P, Rest]} ->
            Part = to_ref_part(P),
            RefParts = lists:reverse([Part | Acc]),
            Ref = #ref{parts=RefParts},
            {ok, Ref, Rest};
        nomatch when Acc =:= [] ->
            none;
        nomatch ->
            throw({illegal_ref, {rest, S}, {parts_so_far, Acc}}) 
    end.

to_ref_part(S) ->
    try list_to_integer(S)
    catch 
        error:badarg ->
            S
    end.

scan_next(S, Regex) ->
    match(S, "^(" ++ Regex ++ ")(.*)$").

%%------------------------------------------------------------------------
%% Misc.
%%------------------------------------------------------------------------
    
strip_edge_space(T) ->
    replace(T, "^\\s*(.*?)\\s*$", "\\1").

new_filter([$_ | _], _Arg) ->
    throw({invalid_filter, "Filter name cannot start with '_'"});
new_filter(Name, Arg) ->
    #filter{name=Name, arg=Arg}.

replace(Subject, Regex, Replacement) ->
    RetType =
        case is_binary(Subject) of
            true ->
                binary;
            false ->
                list
        end,
    re:replace(Subject, Regex, Replacement, 
                    [unicode, dotall, {return, RetType}]).

match(S, Regex) ->
    re:run(S, Regex, [unicode, dotall, {capture, all_but_first, list}]).

split(S, Regex, Opts) ->
    etcher_util:split(S, Regex, Opts).

