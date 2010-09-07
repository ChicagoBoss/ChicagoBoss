-module(boss_mail_driver_mock).
-export([start/0, stop/0, deliver/3, read/2, push/0, pop/0]).

start() ->
    register(boss_mock_inbox, spawn(fun() -> loop([]) end)),
    ok.

stop() ->
    ok.

loop([]) ->
    loop([dict:new()]);
loop([InboxDict|OldState] = State) ->
    receive
        {From, {read, ToAddress, Subject}} ->
            Inbox = case dict:find(ToAddress, InboxDict) of
                {ok, Val} -> Val;
                _ -> []
            end,
            case first_email_with_subject(Subject, Inbox) of
                undefined ->
                    From ! {boss_mock_inbox, undefined},
                    loop(State);
                Email ->
                    NewInbox = lists:delete(Email, Inbox),
                    From ! {boss_mock_inbox, Email},
                    loop([dict:store(ToAddress, NewInbox, InboxDict)|OldState])
            end;
        {From, {deliver, _FromAddress, ToAddress, Email}} ->
            Inbox = case dict:find(ToAddress, InboxDict) of
                {ok, Val} -> Val;
                _ -> []
            end,
            DecodableEmail = iolist_to_binary(Email),
            ParsedEmail = mimemail:decode(DecodableEmail),
            From ! {boss_mock_inbox, ok},
            loop([dict:store(ToAddress, [ParsedEmail|Inbox], InboxDict)|OldState]);
        {From, push} ->
            From ! {boss_mock_inbox, ok},
            loop([InboxDict|State]);
        {From, pop} ->
            From ! {boss_mock_inbox, ok},
            loop(OldState)
    end.

deliver(FromAddress, ToAddress, BodyFun) ->
    boss_mock_inbox ! {self(), {deliver, FromAddress, ToAddress, BodyFun()}},
    receive
        {boss_mock_inbox, ok} ->
            ok
    end.

read(ToAddress, Subject) ->
    boss_mock_inbox ! {self(), {read, ToAddress, Subject}},
    receive
        {boss_mock_inbox, Email} ->
            Email
    end.

push() ->
    boss_mock_inbox ! {self(), push},
    receive
        {boss_mock_inbox, ok} ->
            ok
    end.

pop() ->
    boss_mock_inbox ! {self(), pop},
    receive
        {boss_mock_inbox, ok} ->
            ok
    end.

% internal

first_email_with_subject(Subject, Inbox) when is_list(Subject) ->
    first_email_with_subject(list_to_binary(Subject), Inbox);
first_email_with_subject(_Subject, []) ->
    undefined;
first_email_with_subject(Subject, [{_, _, Headers, _, _} = Email|Rest]) ->
    case proplists:get_value(<<"Subject">>, Headers) of
        Subject -> Email;
        _ -> first_email_with_subject(Subject, Rest)
    end.
