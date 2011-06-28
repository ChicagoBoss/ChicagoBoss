% Module for extracting blocktrans blocks with original source formatting preserved.

-module(blocktrans_scanner).

-export([scan/1]).

scan(Template) ->
    scan(Template, [], {1, 1}, in_text).

scan([], Scanned, _, in_text) ->
    {ok, lists:reverse(lists:map(
                fun
                    ({text, Pos, Text}) ->
                        {text, Pos, lists:reverse(Text)};
                    (Other) ->
                        Other
                end, Scanned))};

scan([], _Scanned, _, {in_comment, _}) ->
    {error, "Reached end of file inside a comment."};

scan([], _Scanned, _, _) ->
    {error, "Reached end of file inside a code block."};

scan("<!--{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text("<!--{{", {Row, Column}, Scanned), {Row, Column + length("<!--{{")}, {in_code, "}}-->"});

scan("{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text("{{", {Row, Column}, Scanned), {Row, Column + 2}, {in_code, "}}"});

scan("<!--{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text("<!--{#", {Row, Column}, Scanned), {Row, Column + length("<!--{#")}, {in_comment, "#}-->"});

scan("{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text("{#", {Row, Column}, Scanned), {Row, Column + length("{#")}, {in_comment, "#}"});

scan("#}-->" ++ T, Scanned, {Row, Column}, {in_comment, "#}-->"}) ->
    scan(T, append_text("#}-->", {Row, Column}, Scanned), {Row, Column + length("#}-->")}, in_text);

scan("#}" ++ T, Scanned, {Row, Column}, {in_comment, "#}"}) ->
    scan(T, append_text("#}", {Row, Column}, Scanned), {Row, Column + length("#}")}, in_text);

scan("<!--{% blocktrans " ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_blocktrans, {Row, Column}, ""} | Scanned], 
        {Row, Column + length("<!--{% blocktrans ")}, {in_code, "%}-->"});

scan("{% blocktrans " ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_blocktrans, {Row, Column}, ""} | Scanned], 
        {Row, Column + length("{% blocktrans ")}, {in_code, "%}"});

scan("{% endblocktrans %}" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{close_blocktrans, {Row, Column}} | Scanned],
        {Row, Column + length("{% endblocktrans %}")}, in_text);

scan("<!--{% endblocktrans %}-->" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{close_blocktrans, {Row, Column}} | Scanned],
        {Row, Column + length("<!--{% endblocktrans %}-->")}, {in_text});

scan("<!--{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text("<!--{%", {Row, Column}, Scanned),
        {Row, Column + length("<!--{%")}, {in_code, "%}-->"});

scan("{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text("{%", {Row, Column}, Scanned),
        {Row, Column + length("{%")}, {in_code, "%}"});

scan([H | T], Scanned, {Row, Column}, {in_comment, Closer}) ->
    scan(T, append_text([H], {Row, Column}, Scanned), {Row, Column + 1}, {in_comment, Closer});

scan("\n" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text("\n", {Row, Column}, Scanned), {Row + 1, 1}, in_text);

scan([H | T], Scanned, {Row, Column}, in_text) ->
    scan(T, append_text([H], {Row, Column}, Scanned), {Row, Column + 1}, in_text);

scan("\"" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, append_text("\"", {Row, Column}, Scanned), {Row, Column + 1} , {in_double_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, append_text("\'", {Row, Column}, Scanned), {Row, Column + 1}, {in_single_quote, Closer});

scan("\\" ++ T, Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_text("\\", {Row, Column}, Scanned), {Row, Column + 1}, {in_double_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote_slash, Closer}) ->
    scan(T, append_text([H], {Row, Column}, Scanned), {Row, Column + 1}, {in_double_quote, Closer});

scan("\\" ++ T, Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_text("\\", {Row, Column}, Scanned), {Row, Column + 1}, {in_single_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote_slash, Closer}) ->
    scan(T, append_text([H], {Row, Column}, Scanned), {Row, Column + 1}, {in_single_quote, Closer});

% end quote
scan("\"" ++ T, Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_text("\"", {Row, Column}, Scanned), {Row, Column + 1}, {in_code, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_text("\'", {Row, Column}, Scanned), {Row, Column + 1}, {in_code, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_text([H], {Row, Column}, Scanned), {Row, Column + 1}, {in_double_quote, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_text([H], {Row, Column}, Scanned), {Row, Column + 1}, {in_single_quote, Closer});


scan("}}-->" ++ T, Scanned, {Row, Column}, {_, "}}-->"}) ->
    scan(T, append_text("}}-->", {Row, Column}, Scanned),
        {Row, Column + length("}}-->")}, in_text);

scan("}}" ++ T, Scanned, {Row, Column}, {_, "}}"}) ->
    scan(T, append_text("}}", {Row, Column}, Scanned), {Row, Column + length("}}")}, in_text);

scan("%}-->" ++ T, Scanned, {Row, Column}, {_, "%}-->"}) ->
    scan(T, append_text("%}-->", {Row, Column}, Scanned),
        {Row, Column + length("%}-->")}, in_text);

scan("%}" ++ T, Scanned, {Row, Column}, {_, "%}"}) ->
    scan(T, append_text("%}", {Row, Column}, Scanned),
        {Row, Column + length("%}")}, in_text);


scan([H | T], Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, append_text([H], {Row, Column}, Scanned), {Row, Column + 1}, {in_code, Closer}).

% internal functions

append_text(Text, Pos, []) ->
    [{text, Pos, Text}];
append_text(Text, Pos, [{close_blocktrans, _}|_] = Scanned) ->
    [{text, Pos, Text}|Scanned];
append_text([C], _Pos, [{open_blocktrans, BPos, ""}|Rest]) when ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) or (C =:= $_) ->
    [{open_blocktrans, BPos, [C]}|Rest];
append_text(" ", _Pos, [{open_blocktrans, BPos, ""}|Rest]) ->
    [{open_blocktrans, BPos, ""}|Rest];
append_text([C], _Pos, [{open_blocktrans, BPos, Name}|Rest]) when is_list(Name) andalso ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) or (C =:= $_) orelse (C >= $0 andalso C =< $9) ->
    [{open_blocktrans, BPos, [C|Name]}|Rest];
append_text(" ", _Pos, [{open_blocktrans, BPos, Name}|Rest]) when is_list(Name) ->
    [{open_blocktrans, BPos, list_to_atom(lists:reverse(Name))}|Rest];
append_text("%}", {Row, Column}, [{open_blocktrans, _BPos, _Name}|_] = Scanned) ->
    [{text, {Row, Column + 2}, ""}|Scanned];
append_text(_Chars, _Pos, [{open_blocktrans, _BPos, _Name}|_] = Scanned) ->
    Scanned;
append_text(Chars, _Pos, [{text, TPos, TChars}|Rest]) ->
    [{text, TPos, lists:reverse(Chars, TChars)}|Rest].
