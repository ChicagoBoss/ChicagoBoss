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
    NewSubscribers = [Subscriber|State#state.subscribers],
    gen_server:reply(From, {ok, erlang:now()}),
    {noreply, State#state{subscribers = NewSubscribers}};
handle_cast({From, pull, 'last', Subscriber}, State) ->
    {NewSubscribers, LastPull} = pull_messages(State#state.last_pull, Subscriber, State),
    gen_server:reply(From, {ok, LastPull}),
    {noreply, State#state{subscribers = NewSubscribers, last_pull = LastPull}};
handle_cast({From, pull, Timestamp, Subscriber}, State) ->
    {NewSubscribers, LastPull} = pull_messages(Timestamp, Subscriber, State),
    gen_server:reply(From, {ok, LastPull}),
    {noreply, State#state{subscribers = NewSubscribers, last_pull = LastPull}};
handle_cast({From, poll, undefined}, State) ->
    Now = erlang:now(),
    gen_server:reply(From, {ok, Now, lists:map(fun({Msg, _Ts}) -> Msg end, State#state.messages)}),
    {noreply, State#state{ last_pull = Now }};
handle_cast({From, poll, 'last'}, State) ->
    ReturnMessages = messages_newer_than_timestamp(State#state.last_pull, State#state.messages),
    Now = erlang:now(),
    gen_server:reply(From, {ok, Now, lists:map(fun({Msg, _Ts}) -> Msg end, ReturnMessages)}),
    {noreply, State#state{ last_pull = Now }};
handle_cast({From, poll, Timestamp}, State) ->
    ReturnMessages = messages_newer_than_timestamp(Timestamp, State#state.messages),
    Now = erlang:now(),
    gen_server:reply(From, {ok, Now, lists:map(fun({Msg, _Ts}) -> Msg end, ReturnMessages)}),
    {noreply, State#state{ last_pull = Now }};
handle_cast({From, push, Message}, State) ->
    Now = erlang:now(),
    LastPull = lists:foldr(fun(Sub, _) ->
                Sub ! {self(), Now, [Message]},
                Now
        end, State#state.last_pull, State#state.subscribers),
    gen_server:reply(From, {ok, Now}),
    CurrentMessages = messages_newer_than_timestamp(now_minus_seconds(erlang:now(), 
            State#state.max_age), State#state.messages),
    NewMessages = [{Message, Now}|CurrentMessages],
    {noreply, State#state{messages = NewMessages, subscribers = [], last_pull = LastPull}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(timeout, State) ->
    gen_server:cast(bmq, {expire, State#state.channel}),
    exit(State#state.supervisor),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


now_minus_seconds({MegaSecs, Secs, MicroSecs}, Delta) ->
    TotalSeconds = MegaSecs * 1000 * 1000 + Secs - Delta,
    {TotalSeconds div (1000 * 1000), TotalSeconds rem (1000 * 1000), MicroSecs}.

messages_newer_than_timestamp(Timestamp, Messages) ->
    messages_newer_than_timestamp(Timestamp, Messages, []).


messages_newer_than_timestamp(_, [], Acc) ->
    lists:reverse(Acc);
messages_newer_than_timestamp({L1, M1, S1} = Ts1, [{Msg, {L2, M2, S2} = Ts2}|Rest], Acc) when (L2 > L1 orelse (L2 =:= L1 andalso (M2 > M1 orelse (M2 =:= M1 andalso S2 > S1)))) ->
    messages_newer_than_timestamp(Ts1, Rest, [{Msg, Ts2}|Acc]);
messages_newer_than_timestamp(_, _, Acc) ->
    lists:reverse(Acc).

pull_messages(Timestamp, Subscriber, State) ->
    Now = erlang:now(),
    case messages_newer_than_timestamp(Timestamp, State#state.messages) of
        ReturnMessages when erlang:length(ReturnMessages) > 0 ->
            Subscriber ! {self(), Now, lists:map(fun({Msg, _Ts}) -> Msg end, ReturnMessages)},
            {State#state.subscribers, Now};
        _ ->
            {[Subscriber|State#state.subscribers], Now}
    end.
