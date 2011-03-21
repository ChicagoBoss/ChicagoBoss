%% @doc Chicago Boss messaging abstraction

-module(boss_mq).

-export([start/0, start/1, stop/0]).

-export([
        pull/1,
        pull/2,
        poll/1,
        poll/2,
        push/2]).

start() ->
    MQOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [mq_port, mq_host, mq_max_age]),
    MQAdapter = case application:get_env(mq_adapter) of
        {ok, Val} -> Val;
        _ -> bmq
    end,
    MQOptions1 = [{adapter, list_to_atom("boss_mq_adapter_"++atom_to_list(MQAdapter))}|MQOptions],
    start(MQOptions1).

start(Options) ->
    boss_mq_sup:start_link(Options).

stop() ->
    ok.

%% @spec pull( Channel::string() ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Pull messages from the specified `Channel'. If none are in the queue, blocks
%% until a message is pushed to the queue.
pull(Channel) ->
    pull(Channel, undefined).

%% @spec pull( Channel::string(), Since::tuple() | now ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Pull messages from the specified `Channel' after `Since' (an `erlang:now/0' tuple). If no such messages
%% are in the queue, blocks until a message is pushed to the queue.
pull(Channel, Timestamp) when is_list(Channel) ->
    case gen_server:call(boss_mq, {pull, Channel, Timestamp, self()}) of
        ok ->
            receive
                {_From, NewTimestamp, Messages} ->
                    {ok, NewTimestamp, Messages}
            end;
        Error ->
            Error
    end;
pull(_, _) ->
    {error, invalid_channel}.

%% @spec poll( Channel::string() ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Like `pull/1', but returns immediately if no messages are in the queue.
poll(Channel) ->
    poll(Channel, undefined).

%% @spec poll( Channel::string(), Since::tuple() ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Like `pull/2', but returns immediately if no matching messages are in the queue.
poll(Channel, Timestamp) when is_list(Channel) ->
    gen_server:call(boss_mq, {poll, Channel, Timestamp});
poll(_, _) ->
    {error, invalid_channel}.

%% @spec push( Channel::string(), Message ) -> ok
%% @doc Pushes a message to the specified `Channel'.
push(Channel, Message) when is_list(Channel) ->
    gen_server:call(boss_mq, {push, Channel, Message});
push(_, _) ->
    {error, invalid_channels}.
