%%%-------------------------------------------------------------------
%%% File:      erlydtl_filters.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc 
%%% Template filters
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
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_filters).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-export([add/2, capfirst/1, center/2, date/2, escapejs/1, first/1]).
-export([fix_ampersands/1, force_escape/1, format_integer/1, format_number/1]).
-export([join/2, last/1, length/1, length_is/2, linebreaksbr/1, ljust/2]).
-export([lower/1, rjust/2, upper/1, urlencode/1]).

-define(NO_ENCODE(C), ((C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        (C =:= $\. orelse C =:= $- 
        orelse C =:= $~ orelse C =:= $_))).

%% @doc Adds a number to the value.
add([Input], Number) when is_list(Input) or is_binary(Input) ->
    add(Input, Number);
add(Input, Number) when is_binary(Input) ->
    list_to_binary(add(binary_to_list(Input), Number));
add(Input, Number) when is_list(Input) ->
    integer_to_list(add(list_to_integer(Input), Number));
add(Input, Number) when is_integer(Input) ->
    Input + Number.

%% @doc Capitalizes the first character of the value.
capfirst([Input]) when is_list(Input) or is_binary (Input) ->
    capfirst(Input);
capfirst([H|T]) when H >= $a andalso H =< $z ->
    [H + $A - $a | T];
capfirst(<<Byte:8/integer, Binary/binary>>) when Byte >= $a andalso Byte =< $z ->
    [<<(Byte + $A - $a)>>, Binary].

%% @doc Centers the value in a field of a given width.
center([Input], Number) when is_list(Input) or is_binary(Input) ->
    center(Input, Number);
center(Input, Number) when is_binary(Input) ->
    list_to_binary(center(binary_to_list(Input), Number));
center(Input, Number) when is_list(Input) ->
    string:centre(Input, Number).

%% @doc Formats a date according to the given format.
date([Input], FormatStr) when is_list(Input) or is_binary(Input) ->
    date(Input, FormatStr);
date(Input, FormatStr) when is_binary(Input) ->
    list_to_binary(date(binary_to_list(Input), FormatStr));
date([{{_,_,_} = Date,{_,_,_} = Time}], FormatStr) ->
    erlydtl_dateformat:format({Date, Time}, FormatStr);
date([{_,_,_} = Date], FormatStr) ->
    erlydtl_dateformat:format(Date, FormatStr);
date(Input, _FormatStr) when is_list(Input) ->
    io:format("Unexpected date parameter : ~p~n", [Input]),
    "".

%% @doc Escapes characters for use in JavaScript strings.
escapejs([Input]) when is_list(Input) or is_binary(Input) ->
    escapejs(Input);
escapejs(Input) when is_binary(Input) ->
    escapejs(Input, 0);
escapejs(Input) when is_list(Input) ->
    escapejs(Input, []).

%% @doc Returns the first item in a list.
first([Input]) when is_list(Input) or is_binary(Input) ->
    first(Input);
first([First|_Rest]) ->
    [First];
first(<<First, _/binary>>) ->
    <<First>>.

%% @doc Replaces ampersands with &amp; entities.
fix_ampersands([Input]) when is_list(Input) or is_binary(Input) ->
    fix_ampersands(Input);
fix_ampersands(Input) when is_binary(Input) ->
    fix_ampersands(Input, 0);
fix_ampersands(Input) when is_list(Input) ->
    fix_ampersands(Input, []).

%% @doc Applies HTML escaping to a string.
force_escape([Input]) when is_list(Input) or is_binary(Input) ->
    force_escape(Input);
force_escape(Input) when is_list(Input) ->
    escape(Input, []);
force_escape(Input) when is_binary(Input) ->
    escape(Input, 0).

format_integer(Input) when is_integer(Input) ->
    integer_to_list(Input);
format_integer(Input) ->
    Input.

format_number(Input) when is_integer(Input) ->
    integer_to_list(Input);
format_number(Input) when is_float(Input) ->
    io_lib:format("~p", [Input]);
format_number(Input) when is_function(Input, 0) ->
    format_number(Input());
format_number(Input) ->
    Input.

%% @doc Joins a list with a given separator.
join([Input], Separator) when is_list(Input) ->
    join_io(Input, Separator).

%% @doc Returns the last item in a list.
last([Input]) when is_list(Input) or is_binary(Input) ->
    last(Input);
last(Input) when is_binary(Input) ->
    case size(Input) of
        0 -> Input;
        N ->
            Offset = N - 1,
            <<_:Offset/binary, Byte/binary>> = Input,
            Byte
    end;
last(Input) when is_list(Input) ->
    [lists:last(Input)].

%% @doc Returns the length of the value.
length([]) -> "0";
length([Input]) when is_list(Input) ->
    integer_to_list(erlang:length(Input));
length([Input]) when is_binary(Input) ->
    integer_to_list(size(Input)).

%% @doc Returns True iff the value's length is the argument.
length_is(Input, Number) when is_list(Input), is_integer(Number) ->
    length_is(Input, integer_to_list(Number));
length_is(Input, Number) when is_list(Input), is_list(Number) ->
    ?MODULE:length(Input) =:= Number.

%% @doc Converts all newlines to HTML line breaks.
linebreaksbr([Input]) when is_list(Input) or is_binary(Input) ->
    linebreaksbr(Input);
linebreaksbr(Input) when is_binary(Input) ->
    linebreaksbr(Input, 0);
linebreaksbr(Input) ->
    linebreaksbr(Input, []).

%% @doc Left-aligns the value in a field of a given width.
ljust([Input], Number) when is_list(Input) or is_binary(Input) ->
    ljust(Input, Number);
ljust(Input, Number) when is_binary(Input) ->
    list_to_binary(ljust(binary_to_list(Input), Number));
ljust(Input, Number) when is_list(Input) ->
    string:left(Input, Number).

%% @doc Converts a string into all lowercase.
lower([Input]) when is_list(Input) or is_binary(Input) ->
    lower(Input);
lower(Input) when is_binary(Input) ->
    lower(Input, 0);
lower(Input) ->
    string:to_lower(Input).

%% @doc Right-aligns the value in a field of a given width.
rjust([Input], Number) when is_list(Input) or is_binary(Input) ->
    rjust(Input, Number);
rjust(Input, Number) when is_binary(Input) ->
    list_to_binary(rjust(binary_to_list(Input), Number));
rjust(Input, Number) ->
    string:right(Input, Number).

%% @doc Converts a string into all uppercase.
upper([Input]) when is_list(Input) or is_binary(Input) ->
    upper(Input);
upper(Input) when is_binary(Input) ->
    list_to_binary(upper(binary_to_list(Input)));
upper(Input) ->
    string:to_upper(Input).

%% @doc Escapes a value for use in a URL.
urlencode([Input]) when is_list(Input) or is_binary(Input) ->
    urlencode(Input);
urlencode(Input) when is_binary(Input) ->
    urlencode(Input, 0);
urlencode(Input) when is_list(Input) ->
    urlencode(Input, []).

% internal

escape(Binary, Index) when is_binary(Binary) ->
    case Binary of
        <<Pre:Index/binary, $<, Post/binary>> ->
            process_binary_match(Pre, <<"&lt;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, $>, Post/binary>> ->
            process_binary_match(Pre, <<"&gt;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, 34, Post/binary>> ->
            process_binary_match(Pre, <<"&quot;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, 39, Post/binary>> ->
            process_binary_match(Pre, <<"&#039;">>, size(Post), escape(Post, 0));
        <<_:Index/binary, _, _/binary>> ->
            escape(Binary, Index + 1);
        Binary ->
            Binary
    end;
escape([], Acc) ->
    lists:reverse(Acc);
escape("<" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape(">" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape("&" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape("\"" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&quot;", Acc));
escape("'" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&#039;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).


escapejs([], Acc) ->
    lists:reverse(Acc);
escapejs("'" ++ Rest, Acc) ->
    escapejs(Rest, lists:reverse("\\'", Acc));
escapejs("\"" ++ Rest, Acc) ->
    escapejs(Rest, lists:reverse("\\\"", Acc));
escapejs([C | Rest], Acc) ->
    escapejs(Rest, [C | Acc]);
escapejs(Binary, Index) when is_binary(Binary) ->
    case Binary of
        <<Pre:Index/binary, 39, Post/binary>> ->
            process_binary_match(Pre, <<"\\'">>, size(Post), escapejs(Post, 0));
        <<Pre:Index/binary, 34, Post/binary>> ->
            process_binary_match(Pre, <<"\\\"">>, size(Post), escapejs(Post, 0));
        <<_:Index/binary, _/binary>> ->
            escapejs(Binary, Index + 1);
        _ ->
            Binary
    end.

fix_ampersands(Input, Index) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), Post);
        <<_:Index/binary, _/binary>> ->
            fix_ampersands(Input, Index + 1);
        _ ->
            Input
    end;
fix_ampersands([], Acc) ->
    lists:reverse(Acc);
fix_ampersands("&" ++ Rest, Acc) ->
    fix_ampersands(Rest, lists:reverse("&amp;", Acc));
fix_ampersands([C | Rest], Acc) ->
    fix_ampersands(Rest, [C | Acc]).

join_io([], _Sep) -> [];
join_io([_] = X, _Sep) -> X;
join_io([X|T], Sep) -> [X,Sep] ++ join_io(T, Sep).

linebreaksbr(Input, Index) when is_binary(Input) ->
    Break = <<"<br />">>,
    case Input of
        <<Pre:Index/binary, $\r, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr(Post, 0));
        <<Pre:Index/binary, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr(Post, 0));
        <<_:Index/binary, _/binary>> ->
            linebreaksbr(Input, Index + 1);
        _ ->
            Input
    end;
linebreaksbr([], Acc) ->
    lists:reverse(Acc);
linebreaksbr("\r\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr("\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr([C | Rest], Acc) ->
    linebreaksbr(Rest, [C | Acc]).

lower(Input, Index) ->
    case Input of
        <<Pre:Index/binary, Byte, Post/binary>> when Byte >= $A andalso Byte =< $Z ->
            process_binary_match(Pre, <<(Byte - $A + $a)>>, size(Post), lower(Post, 0));
        <<_:Index/binary, _/binary>> ->
            lower(Input, Index + 1);
        _ ->
            Input
    end.

% Taken from quote_plus of mochiweb_util

urlencode(Input, Index) when is_binary(Input) ->
    case Input of
        <<_:Index/binary, Byte, _/binary>> when ?NO_ENCODE(Byte) ->
            urlencode(Input, Index + 1);
        <<Pre:Index/binary, $\s, Post/binary>> ->
            process_binary_match(Pre, <<"+">>, size(Post), urlencode(Post, 0));
        <<Pre:Index/binary, Hi:4, Lo:4, Post/binary>> ->
            HiDigit = hexdigit(Hi),
            LoDigit = hexdigit(Lo),
            Code = <<$\%, HiDigit, LoDigit>>,
            process_binary_match(Pre, Code, size(Post), urlencode(Post, 0));
        Input ->
            Input
    end;
urlencode([], Acc) ->
    lists:reverse(Acc);
urlencode([C | Rest], Acc) when ?NO_ENCODE(C) ->
    urlencode(Rest, [C | Acc]);
urlencode([$\s | Rest], Acc) ->
    urlencode(Rest, [$+ | Acc]);
urlencode([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    urlencode(Rest, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.
