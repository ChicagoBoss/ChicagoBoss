-module(bmq_channel_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {channel, messages = [], subscribers = [], max_age, supervisor}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    MaxAgeSeconds = proplists:get_value(max_age, Options, 60),
    Supervisor = proplists:get_value(supervisor, Options),
    Channel = proplists:get_value(channel, Options),
    {ok, #state{max_age = MaxAgeSeconds, supervisor = Supervisor, channel = Channel}, 
        MaxAgeSeconds * 1000}.

handle_call(_From, _, State) ->
    {noreply, State}.

handle_cast({From, pull, 'now', Subscriber}, State) ->
    NewSubscribers = [Subscriber|State#state.subscribers],
    gen_server:reply(From, ok),
    {noreply, State#state{subscribers = NewSubscribers}};
handle_cast({From, pull, Timestamp, Subscriber}, State) ->
    NewSubscribers = case messages_newer_than_timestamp(Timestamp, State#state.messages) of
        ReturnMessages when erlang:length(ReturnMessages) > 0 ->
            Subscriber ! {self(), erlang:now(), ReturnMessages},
            State#state.subscribers;
        _ ->
            [Subscriber|State#state.subscribers]
    end,
    gen_server:reply(From, ok),
    {noreply, State#state{subscribers = NewSubscribers}};
handle_cast({From, poll, undefined}, State) ->
    gen_server:reply(From, {ok, erlang:now(), State#state.messages}),
    {noreply, State};
handle_cast({From, poll, Timestamp}, State) ->
    ReturnMessages = messages_newer_than_timestamp(Timestamp, State#state.messages),
    gen_server:reply(From, {ok, erlang:now(), ReturnMessages}),
    {noreply, State};
handle_cast({From, push, Message}, State) ->
    Now = erlang:now(),
    lists:map(fun(Sub) ->
                Sub ! {self(), Now, [Message]}
        end, State#state.subscribers),
    gen_server:reply(From, {ok, Now}),
    CurrentMessages = messages_newer_than_timestamp(now_minus_seconds(erlang:now(), 
            State#state.max_age), State#state.messages),
    NewMessages = [{Message, Now}|CurrentMessages],
    {noreply, State#state{messages = NewMessages, subscribers = []}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(timeout, State) ->
    io:format("Channel exiting: ~p~n", [State#state.channel]),
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
messages_newer_than_timestamp({L1, M1, S1} = Ts, [{Msg, {L2, M2, S2}}|Rest], Acc) when (L2 > L1 orelse (L2 =:= L1 andalso (M2 > M1 orelse (M2 =:= M1 andalso S2 > S1)))) ->
    messages_newer_than_timestamp(Ts, Rest, [Msg|Acc]);
messages_newer_than_timestamp(_, _, Acc) ->
    lists:reverse(Acc).

