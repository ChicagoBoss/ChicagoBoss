% In-memory message queue. Should work fine for 1-machine chat apps

-module(boss_mq_adapter_mock).
-export([start/0, start/1, stop/1]).
-export([pull/4, poll/3, push/3]).

start() ->
    start([]).

start(Options) ->
    MaxAgeSeconds = proplists:get_value(mq_max_age, Options, 5),
    register(boss_mq_mock, spawn(fun() -> loop({dict:new(), dict:new()}, MaxAgeSeconds * 1000) end)),
    {ok, undefined}.

stop(_) ->
    boss_mq_mock ! {self(), stop},
    ok.

loop({Chan2Pid, Pid2Chan} = Dict, MaxAge) ->
    receive
        {'EXIT', Pid, normal} ->
            error_logger:info_msg("Process exited~n"),
            {ok, Channel} = dict:find(Pid, Pid2Chan),
            NewDict = {dict:erase(Channel, Chan2Pid), dict:erase(Pid, Pid2Chan)},
            loop(NewDict, MaxAge);
        {From, pull, Channel, Timestamp, Subscriber} ->
            {ChannelPid, NewDict} = find_or_create_channel(Channel, Dict, MaxAge),
            ChannelPid ! {From, pull, Timestamp, Subscriber},
            loop(NewDict, MaxAge);
        {From, poll, Channel, Timestamp} ->
            case dict:find(Channel, Chan2Pid) of
                {ok, ChannelPid} -> ChannelPid ! {From, poll, Timestamp};
                _ -> From ! {self(), erlang:now(), []}
            end,
            loop(Dict, MaxAge);
        {From, push, Channel, Message} ->
            {ChannelPid, NewDict} = find_or_create_channel(Channel, Dict, MaxAge),
            ChannelPid ! {From, push, Message},
            loop(NewDict, MaxAge);
        {_From, stop} ->
            ok;
        Other ->
            error_logger:info_msg("Unrecognized message: ~p~n", [Other]),
            loop(Dict, MaxAge)
    end.

channel_loop({Messages, Subscribers}, MaxAge) ->
    receive
        {From, pull, 'now', Subscriber} ->
            From ! {self(), ok},
            channel_loop({Messages, [Subscriber|Subscribers]}, MaxAge);
        {From, pull, Timestamp, Subscriber} ->
            NewSubscribers = case messages_newer_than_timestamp(Timestamp, Messages) of
                ReturnMessages when erlang:length(ReturnMessages) > 0 ->
                    Subscriber ! {self(), erlang:now(), ReturnMessages},
                    Subscribers;
                _ -> 
                    [Subscriber|Subscribers]
            end,
            From ! {self(), ok},
            channel_loop({Messages, NewSubscribers}, MaxAge);
        {From, poll, Timestamp} ->
            ReturnMessages = messages_newer_than_timestamp(Timestamp, Messages),
            From ! {self(), erlang:now(), ReturnMessages},
            channel_loop({Messages, Subscribers}, MaxAge);
        {From, push, Msg} ->
            Now = erlang:now(),
            lists:map(fun(Sub) ->
                        Sub ! {self(), Now, [Msg]}
                end, Subscribers),
            From ! {self(), Now},
            channel_loop({[{Msg, Now}|Messages], []}, MaxAge);
        Other ->
            error_logger:info_msg("Unrecognized message: ~p~n", [Other]),
            channel_loop({Messages, Subscribers}, MaxAge)
    after 
        MaxAge ->
            case erlang:length(Subscribers) of
                0 -> 
                    boss_mq_mock ! {'EXIT', self(), normal},
                    exit(normal);
                _ -> channel_loop({[], Subscribers}, MaxAge)
            end
    end.


pull(_, Channel, Timestamp, Subscriber) ->
    boss_mq_mock ! {self(), pull, Channel, Timestamp, Subscriber},
    receive
        {_From, ok} ->
            ok
    end.

poll(_, Channel, Timestamp) ->
    boss_mq_mock ! {self(), poll, Channel, Timestamp},
    receive
        {_From, NewTimestamp, Messages} ->
            {ok, NewTimestamp, Messages}
    end.

push(_, Channel, Message) ->
    boss_mq_mock ! {self(), push, Channel, Message},
    receive
        {_From, _Timestamp} ->
            ok
    end.




% internal

messages_newer_than_timestamp(Timestamp, Messages) ->
    messages_newer_than_timestamp(Timestamp, Messages, []).


messages_newer_than_timestamp(_, [], Acc) ->
    lists:reverse(Acc);
messages_newer_than_timestamp({L1, M1, S1} = Ts, [{Msg, {L2, M2, S2}}|Rest], Acc) when (L2 > L1 orelse (L2 =:= L1 andalso (M2 > M1 orelse (M2 =:= M1 andalso S2 > S1)))) ->
    messages_newer_than_timestamp(Ts, Rest, [Msg|Acc]);
messages_newer_than_timestamp(_, _, Acc) ->
    lists:reverse(Acc).

find_or_create_channel(Channel, {Chan2Pid, Pid2Chan} = Dict, MaxAge) ->
    case dict:find(Channel, Chan2Pid) of
        {ok, Pid} ->
            {Pid, Dict};
        _ ->
            Pid = spawn(fun() -> channel_loop({[], []}, MaxAge) end),
            {Pid, {dict:store(Channel, Pid, Chan2Pid), dict:store(Pid, Channel, Pid2Chan)}}
    end.
