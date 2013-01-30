-module(boss_string).
-export([
         title/1,
         title/2
        ]).

%% This set of tokens should match the set used by PHP's str.title()
%% in order to capitalize as the Django title filter.
-define(TITLE_DEFAULT_TOKENS, " `-=~!?@\"#$%^&*()_+[]{}'\\|/,.<>:;").

title([]) ->
    [];
title(S) ->
    title(S, ?TITLE_DEFAULT_TOKENS).

title([C|CS], Tokens) ->
    lists:flatten(
      [string:to_upper(C)|title_helper(string:to_lower(CS), Tokens)]
     ).

title_helper([], _Tokens) ->
    [];
title_helper([C|[]], _Tokens) ->
    [C|[]];
title_helper([T|[C|CS] = CSS] = _Word, Tokens) when C >= $a, C =< $z ->
    %% io:format("[~c|[~c|~s] = ~s] = ~s~n", [T, C, CS, CSS, _Word]),
    case lists:member(T, Tokens) of
        true -> [T|[string:to_upper(C)|title_helper(CS, Tokens)]];
        false -> [T|title_helper(CSS, Tokens)]
    end;
title_helper([C|CS], Tokens) ->
    [C|title_helper(CS, Tokens)].


