-module(bmq).

-export([now/1, poll/2, pull/3, push/2].

pull(Channel, Timestamp, Subscriber) ->
    gen_server:call(bmq, {pull, Channel, Timestamp, Subscriber}).

poll(Channel, Timestamp) ->
    gen_server:call(bmq, {poll, Channel, Timestamp}).

push(Channel, Message) ->
    gen_server:call(bmq, {push, Channel, Message}).

now(Channel) ->
    gen_server:call(bmq, {now, Channel}).
