%% @doc Chicago Boss messaging abstraction

-module(boss_mq).

-export([start/0, start/1, stop/0]).

-export([
        pull/1,
        pull/2,
        pull/3,
        poll/1,
        poll/2,
        push/2,
        now/1]).

start() ->
    MQOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [mq_port, mq_host, mq_max_age]),
    MQAdapter = case application:get_env(mq_adapter) of
        {ok, Val} -> Val;
        _ -> tinymq
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

%% @spec pull( Channel::string(), Since::integer() | last | now ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Pull messages from the specified `Channel' after `Since' (a timestamp returned from a previous `pull'). 
%% If no such messages are in the queue, blocks until a message is pushed to the queue.
pull(Channel, Timestamp) ->
    pull(Channel, Timestamp, infinity).

%% @spec pull( Channel::string(), Since::integer() | last | now, Timeout::integer() ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Pull messages from the specified `Channel' after `Since' (a timestamp returned from a previous `pull'). If no such messages
%% are in the queue, blocks until a message is pushed to the queue, or until `Timeout' seconds have elapsed.
pull(Channel, {MegaSecs, Secs, MicroSecs}, Timeout) ->
    pull(Channel, 1000 * 1000 * (1000 * 1000 * MegaSecs + Secs) + MicroSecs, Timeout);
pull(Channel, Timestamp, Timeout) when is_list(Channel) ->
    TimeoutMs = case Timeout of
        infinity -> infinity;
        _ -> Timeout * 1000
    end,
    case gen_server:call({global, ?MODULE}, {pull, Channel, Timestamp, self()}) of
        {ok, PullTime} ->
            receive
                {_From, NewTimestamp, Messages} ->
                    {ok, NewTimestamp, Messages}
            after
                TimeoutMs ->
                    {ok, PullTime, []}
            end;
        Error ->
            Error
    end;
pull(_, _, _) ->
    {error, invalid_channel}.

%% @spec poll( Channel::string() ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Like `pull/1', but returns immediately if no messages are in the queue.
poll(Channel) ->
    poll(Channel, undefined).

%% @spec poll( Channel::string(), Since::integer() | last ) -> {ok, Timestamp, [Message]} | {error, Reason}
%% @doc Like `pull/2', but returns immediately if no matching messages are in the queue.
poll(Channel, Timestamp) when is_list(Channel) ->
    gen_server:call({global, ?MODULE}, {poll, Channel, Timestamp});
poll(_, _) ->
    {error, invalid_channel}.

%% @spec push( Channel::string(), Message ) -> {ok, Timestamp}
%% @doc Pushes a message to the specified `Channel'.
push(Channel, Message) when is_list(Channel) ->
    gen_server:call({global, ?MODULE}, {push, Channel, Message});
push(_, _) ->
    {error, invalid_channel}.

%% @spec now( Channel::string() ) -> Timestamp
%% @doc Retrieves the current time for the server managing `Channel'.
now(Channel) when is_list(Channel) ->
    gen_server:call({global, ?MODULE}, {now, Channel});
now(_) ->
    {error, invalid_channel}.
