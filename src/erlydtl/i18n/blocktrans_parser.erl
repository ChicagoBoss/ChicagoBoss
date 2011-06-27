-module(blocktrans_parser).

-export([parse/1]).

parse(Tokens) ->
    parse(Tokens, []).

parse([], Acc) ->
    lists:reverse(Acc);
parse([{open_blocktrans, _, Name}, {text, _, Text}, {close_blocktrans, _}|Rest], Acc) ->
    parse(Rest, [{Name, unicode:characters_to_binary(Text)}|Acc]);
parse([{text, _, _}|Rest], Acc) ->
    parse(Rest, Acc).
