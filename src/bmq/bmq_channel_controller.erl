-module(bmq_channel_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {channel, messages = [], subscribers = [], max_age, last_pull, supervisor}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    MaxAgeSeconds = proplists:get_value(max_age, Options, 60),
    Supervisor = proplists:get_value(supervisor, Options),
    Channel = proplists:get_value(channel, Options),
    {ok, #state{max_age = MaxAgeSeconds, supervisor = Supervisor, channel = Channel, last_pull = erlang:now()}, 
        MaxAgeSeconds * 1000}.

handle_call(_From, _, State) ->
    {noreply, State}.

handle_cast({From, pull, 'now', Subscriber}, State) ->
    NewSubscribers = [{erlang:monitor(process, Subscriber), Subscriber}|State#state.subscribers],
    gen_server:reply(From, {ok, now_to_micro_seconds(erlang:now())}),
    {noreply, State#state{subscribers = NewSubscribers}};
handle_cast({From, pull, 'last', Subscriber}, State) ->
    {NewSubscribers, LastPull} = pull_messages(State#state.last_pull, Subscriber, State),
    gen_server:reply(From, {ok, LastPull}),
    {noreply, State#state{subscribers = NewSubscribers, last_pull = LastPull}};
handle_cast({From, pull, undefined, Subscriber}, State) ->
    {NewSubscribers, LastPull} = pull_messages(undefined, Subscriber, State),
    gen_server:reply(From, {ok, LastPull}),
    {noreply, State#state{subscribers = NewSubscribers, last_pull = LastPull}};
handle_cast({From, pull, Timestamp, Subscriber}, State) when is_integer(Timestamp) ->
    {NewSubscribers, LastPull} = pull_messages(Timestamp, Subscriber, State),
    gen_server:reply(From, {ok, LastPull}),
    {noreply, State#state{subscribers = NewSubscribers, last_pull = LastPull}};
handle_cast({From, poll, undefined}, State) ->
    Now = now_to_micro_seconds(erlang:now()),
    gen_server:reply(From, {ok, Now, lists:map(fun({Msg, _Ts}) -> Msg end, State#state.messages)}),
    {noreply, State#state{ last_pull = Now }};
handle_cast({From, poll, 'last'}, State) ->
    ReturnMessages = messages_newer_than_timestamp(State#state.last_pull, State#state.messages),
    Now = now_to_micro_seconds(erlang:now()),
    gen_server:reply(From, {ok, Now, lists:map(fun({Msg, _Ts}) -> Msg end, ReturnMessages)}),
    {noreply, State#state{ last_pull = Now }};
handle_cast({From, poll, Timestamp}, State) ->
    ReturnMessages = messages_newer_than_timestamp(Timestamp, State#state.messages),
    Now = now_to_micro_seconds(erlang:now()),
    gen_server:reply(From, {ok, Now, lists:map(fun({Msg, _Ts}) -> Msg end, ReturnMessages)}),
    {noreply, State#state{ last_pull = Now }};
handle_cast({From, push, Message}, State) ->
    Now = now_to_micro_seconds(erlang:now()),
    LastPull = lists:foldr(fun({Ref, Sub}, _) ->
                Sub ! {self(), Now, [Message]},
                erlang:demonitor(Ref),
                Now
        end, State#state.last_pull, State#state.subscribers),
    gen_server:reply(From, {ok, Now}),
    CurrentMessages = messages_newer_than_timestamp(now_to_micro_seconds(erlang:now()) - 
        seconds_to_micro_seconds(State#state.max_age), State#state.messages),
    NewMessages = [{Message, Now}|CurrentMessages],
    {noreply, State#state{messages = NewMessages, subscribers = [], last_pull = LastPull}};
handle_cast({From, now}, State) ->
    gen_server:reply(From, now_to_micro_seconds(erlang:now())),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(timeout, State) ->
    gen_server:cast(bmq, {expire, State#state.channel}),
    exit(State#state.supervisor),
    {noreply, State};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    {noreply, State#state{ subscribers = proplists:delete(Ref, State#state.subscribers) }};
handle_info(_Info, State) ->
    {noreply, State}.


seconds_to_micro_seconds(Seconds) ->
    Seconds * 1000 * 1000.

now_to_micro_seconds({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + MicroSecs.

messages_newer_than_timestamp(undefined, Messages) ->
    Messages;
messages_newer_than_timestamp(Timestamp, Messages) ->
    messages_newer_than_timestamp(Timestamp, Messages, []).

messages_newer_than_timestamp(_, [], Acc) ->
    lists:reverse(Acc);
messages_newer_than_timestamp(Ts1, [{Msg, Ts2}|Rest], Acc) when Ts2 > Ts1 ->
    messages_newer_than_timestamp(Ts1, Rest, [{Msg, Ts2}|Acc]);
messages_newer_than_timestamp(_, _, Acc) ->
    lists:reverse(Acc).

pull_messages(Timestamp, Subscriber, State) ->
    Now = now_to_micro_seconds(erlang:now()),
    case messages_newer_than_timestamp(Timestamp, State#state.messages) of
        ReturnMessages when erlang:length(ReturnMessages) > 0 ->
            Subscriber ! {self(), Now, lists:map(fun({Msg, _Ts}) -> Msg end, ReturnMessages)},
            {State#state.subscribers, Now};
        _ ->
            {[{erlang:monitor(process, Subscriber), Subscriber}|State#state.subscribers], Now}
    end.
