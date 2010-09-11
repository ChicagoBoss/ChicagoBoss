-module(boss_test).
-compile(export_all).

process_assertions_and_continuations(Assertions, Continuations, ParsedResponse) ->
    {NumSuccesses, FailureMessages} = process_assertions(Assertions, ParsedResponse),
    case length(FailureMessages) of
        0 ->
            io:format("~3B passed~n", [NumSuccesses]),
            {NewS, NewF} = process_continuations(Continuations, ParsedResponse),
            {NumSuccesses + NewS, FailureMessages ++ NewF};
        N ->
            io:format("~c[01;31m~3B failed~c[00m~n", [16#1B, N, 16#1B]),
            lists:map(fun(Msg) ->
                        io:format("~s* ~c[01m~p~c[00m~n", 
                            [lists:duplicate(boss_db:depth() - 1, $\ ), 
                                16#1B, Msg, 16#1B])
                end, FailureMessages),
            io:format("Last response: ~p~n~n", [ParsedResponse]),
            io:format("Database: ~p~n~n", [boss_db:dump()]),
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

process_continuations(Continuations, Response) ->
    process_continuations(Continuations, Response, {0, []}).

process_continuations([], _, {NumSuccesses, FailureMessages}) ->
    {NumSuccesses, lists:reverse(FailureMessages)};
process_continuations([Name, Fun | Rest], Response, {NumSuccesses, FailureMessages}) 
        when is_list(Name) and is_function(Fun) ->
    io:format("~-60s", [lists:duplicate(boss_db:depth(), $\ ) ++ Name]),
    boss_db:push(),
    boss_mail_driver_mock:push(),
    {TheseSuccesses, TheseFailureMessages} = Fun(Response),
    boss_mail_driver_mock:pop(),
    boss_db:pop(),
    process_continuations(Rest, Response, {NumSuccesses + TheseSuccesses, TheseFailureMessages ++ FailureMessages}).

