%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc
%%-------------------------------------------------------------------

-module(boss_mq_adapter_tinymq).
-export([start/0, start/1, stop/1]).
-export([pull/4, poll/3, push/3, now/2]).

start() ->
    start([]).

start(_Options) ->
    application:start(tinymq),
    {ok, undefined}.

stop(_) ->
    application:stop(tinymq),
    ok.

pull(_, Channel, Timestamp, Subscriber) ->
    tinymq:subscribe(Channel, Timestamp, Subscriber).

poll(_, Channel, Timestamp) ->
    tinymq:poll(Channel, Timestamp).

push(_, Channel, Message) ->
    tinymq:push(Channel, Message).

now(_, Channel) ->
    tinymq:now(Channel).
