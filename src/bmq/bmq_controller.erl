-module(bmq_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {dict, max_age}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, bmq}, ?MODULE, Args, []).

init(Options) ->
    MaxAgeSeconds = proplists:get_value(max_age, Options, 60),
    {ok, #state{dict = dict:new(), max_age = MaxAgeSeconds}}.

handle_call({pull, Channel, Timestamp, Subscriber}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, pull, Timestamp, Subscriber}),
    {noreply, NewState};

handle_call({poll, Channel, Timestamp}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, poll, Timestamp}),
    {noreply, NewState};

handle_call({push, Channel, Message}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, push, Message}),
    {noreply, NewState};

handle_call({now, Channel}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, now}),
    {noreply, NewState}.

handle_cast({expire, Channel}, State) ->
    NewState = State#state{
        dict = dict:erase(Channel, State#state.dict)},
    {noreply, NewState};

handle_cast({set_max_age, NewMaxAge}, State) ->
    {noreply, State#state{max_age = NewMaxAge}};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.


% internal

find_or_create_channel(Channel, #state{dict = Chan2Pid, max_age = MaxAge} = State) ->
    case dict:find(Channel, Chan2Pid) of
        {ok, Pid} ->
            {Pid, State};
        _ ->
            {ok, ChannelSup} = bmq_channel_sup:start_link(),
            {ok, ChannelPid} = bmq_channel_sup:start_child(ChannelSup, 
                [{max_age, MaxAge}, {supervisor, ChannelSup}, {channel, Channel}]),
            {ChannelPid, State#state{
                    dict = dict:store(Channel, ChannelPid, Chan2Pid)
                }}
    end.
