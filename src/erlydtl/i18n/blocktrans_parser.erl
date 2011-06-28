-module(blocktrans_parser).

-export([parse/1]).

parse(Tokens) ->
    parse(Tokens, []).

parse([], Acc) ->
    clean_crs(lists:reverse(Acc));
parse([{open_blocktrans, _, Name}, {text, _, Text}, {close_blocktrans, _}|Rest], Acc) ->
    parse(Rest, [{Name, unicode:characters_to_binary(Text)}|Acc]);
parse([{text, _, _}|Rest], Acc) ->
    parse(Rest, Acc).

% internal functions

clean_crs(Tokens) ->
	lists:map(fun(X) -> 
			{element(1, X), list_to_binary(string:strip(binary_to_list(element(2, X)), both, $\n))} 
		end, Tokens).