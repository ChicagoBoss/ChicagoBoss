-module(boss_test).
-export([process_assertions_and_continuations/3, process_assertions_and_continuations/6]).

process_assertions_and_continuations(Assertions, Continuations, ParsedResponse) ->
    process_assertions_and_continuations(Assertions, Continuations, ParsedResponse, 
        fun() -> ok end, fun() -> ok end, fun() -> "" end).

process_assertions_and_continuations(Assertions, Continuations, ParsedResponse, PushFun, PopFun, DumpFun) ->
    Depth = case get(boss_test_depth) of
        undefined -> 0;
        D -> D
    end,
    {NumSuccesses, FailureMessages} = process_assertions(Assertions, ParsedResponse),
    case length(FailureMessages) of
        0 ->
            io:format("~3B passed~n", [NumSuccesses]),
            {NewS, NewF} = process_continuations(Continuations, ParsedResponse, PushFun, PopFun, Depth),
            {NumSuccesses + NewS, FailureMessages ++ NewF};
        N ->
            io:format("~c[01;31m~3B failed~c[00m~n", [16#1B, N, 16#1B]),
            lists:map(fun(Msg) ->
                        io:format("~s* ~c[01m~p~c[00m~n", 
                            [lists:duplicate(Depth, $\ ), 
                                16#1B, Msg, 16#1B])
                end, FailureMessages),
            io:format("Last response: ~p~n~n", [ParsedResponse]),
            io:format("Database: ~p~n~n", [DumpFun()]),
            {NumSuccesses, FailureMessages}
    end.

process_assertions(Assertions, ParsedResponse) ->
    lists:foldl(fun
            (AssertionFun, {N, Acc}) when is_function(AssertionFun) ->
                case AssertionFun(ParsedResponse) of
                    {true, _Msg} ->
                        {N+1, Acc};
                    {false, Msg} ->
                        {N, [Msg|Acc]}
                end
        end, {0, []}, Assertions).

process_continuations(Continuations, Response, PushFun, PopFun, Depth) ->
    process_continuations(Continuations, Response, PushFun, PopFun, Depth, {0, []}).

process_continuations([], _, _, _, _, {NumSuccesses, FailureMessages}) ->
    {NumSuccesses, lists:reverse(FailureMessages)};
process_continuations([Name, Fun | Rest], Response, PushFun, PopFun, Depth, {NumSuccesses, FailureMessages}) 
        when is_list(Name) and is_function(Fun) ->
    io:format("~-60s", [lists:duplicate(Depth, $\ ) ++ Name]),
    PushFun(),
    put(boss_test_depth, Depth + 1),
    {TheseSuccesses, TheseFailureMessages} = Fun(Response),
    put(boss_test_depth, Depth),
    PopFun(),
    process_continuations(Rest, Response, PushFun, PopFun, Depth, {NumSuccesses + TheseSuccesses, TheseFailureMessages ++ FailureMessages}).
