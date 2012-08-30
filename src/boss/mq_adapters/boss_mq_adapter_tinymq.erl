-module(boss_mq_adapter_tinymq).
-export([start/0, start/1, stop/1]).
-export([pull/4, poll/3, push/3, now/2]).

start() ->
    MQOptions = case application:get_env(mq_max_age) of
        {ok, Val} -> [{max_age, Val}];
        _ -> []
    end,
    start(MQOptions).

start(Options) ->
    tinymq_sup:start_link(Options),
    {ok, undefined}.

stop(_) ->
    ok.

pull(_, Channel, Timestamp, Subscriber) ->
    tinymq:subscribe(Channel, Timestamp, Subscriber).

poll(_, Channel, Timestamp) ->
    tinymq:poll(Channel, Timestamp).

push(_, Channel, Message) ->
    tinymq:push(Channel, Message).

now(_, Channel) ->
    tinymq:now(Channel).
