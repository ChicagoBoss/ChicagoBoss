-module(bmq).
-compile(export_all).


pull(Channel, Timestamp, Subscriber) ->
    gen_server:call(bmq, {pull, Channel, Timestamp, Subscriber}).

poll(Channel, Timestamp) ->
    gen_server:call(bmq, {poll, Channel, Timestamp}).

push(Channel, Message) ->
    gen_server:call(bmq, {push, Channel, Message}).

now(Channel) ->
    gen_server:call(bmq, {now, Channel}).
