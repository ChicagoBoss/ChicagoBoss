%% @doc Chicago Boss messaging abstraction

-module(boss_mq).

-export([start/0, start/1, stop/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
        pull/1,
        pull/2,
        pull/3,
        poll/1,
        poll/2,
        push/2,
        now/1]).
-type channel()   ::string().
-type mq_return() :: {ok, integer(), [_]}|{error,string()}.

-spec start() -> any().
start() ->
    MQOptions	= make_queue_options(),
    MQAdapter	= get_mq_adapater(),
    MQOptions1	= [{adapter, list_to_atom("boss_mq_adapter_"++atom_to_list(MQAdapter))}|MQOptions],
    start(MQOptions1).

-spec start(_) -> any().
start(Options) ->
    boss_mq_sup:start_link(Options).

-spec stop() -> 'ok'.
stop() ->
    ok.

-spec get_mq_adapater() -> atom().
get_mq_adapater() ->
	boss_env:mq_adapter().

-spec make_queue_options() -> [{atom(), term()}].
make_queue_options() ->
    lists:foldl(fun(OptName, Acc) ->
			case application:get_env(OptName) of
			    {ok, Val} -> [{OptName, Val}|Acc];
			    _ -> Acc
			end
                end,
		[], 
		[mq_port, mq_host, mq_max_age]).



-spec pull(channel()) -> mq_return().
%% @doc Pull messages from the specified `Channel'. If none are in the queue, blocks
%% until a message is pushed to the queue.
pull(Channel) ->
    pull(Channel, undefined).

-spec pull(channel(),undefined|non_neg_integer()) -> mq_return().
%% @doc Pull messages from the specified `Channel' after `Since' (a timestamp returned from a previous `pull'). 
%% If no such messages are in the queue, blocks until a message is pushed to the queue.
pull(Channel, Timestamp) ->
    pull(Channel, Timestamp, infinity).

-spec pull(channel(),undefined|integer()|{integer(), integer(), integer()},infinity|non_neg_integer()) -> mq_return().
%% @doc Pull messages from the specified `Channel' after `Since' (a timestamp returned from a previous `pull'). If no such messages
%% are in the queue, blocks until a message is pushed to the queue, or until `Timeout' seconds have elapsed.
pull(Channel, {MegaSecs, Secs, MicroSecs}, Timeout) ->
    pull(Channel, 1000 * 1000 * (1000 * 1000 * MegaSecs + Secs) + MicroSecs, Timeout);
pull(Channel, Timestamp, Timeout) when is_list(Channel) ->
    TimeoutMs  = convert_to_ms(Timeout),
    CallStatus = gen_server:call({global, ?MODULE}, {pull, Channel, Timestamp, self()}),
    pull_recieve(TimeoutMs, CallStatus).


-spec pull_recieve('infinity' | non_neg_integer(),{ok, _}|any()) -> mq_return().
pull_recieve(TimeoutMs, {ok, PullTime}) ->
    receive
	{_From, NewTimestamp, Messages} ->
	    {ok, NewTimestamp, Messages}
    after
	TimeoutMs ->
	    {ok, PullTime, []}
    end;
pull_recieve(_, Error) -> Error.


-spec convert_to_ms('infinity' | non_neg_integer()) -> 'infinity' | non_neg_integer().
convert_to_ms(infinity) -> infinity;
convert_to_ms(Timeout) ->
    Timeout * 1000.


-spec poll(channel()) -> mq_return().
%% @doc Like `pull/1', but returns immediately if no messages are in the queue.
poll(Channel) ->
    poll(Channel, last).

-spec poll(channel(),last|integer()) -> mq_return().
%% @doc Like `pull/2', but returns immediately if no matching messages are in the queue.
poll(Channel, Timestamp) when is_list(Channel) ->
    gen_server:call({global, ?MODULE}, {poll, Channel, Timestamp}).
    

-spec push(channel(),_) -> {ok, non_neg_integer()}.
%% @doc Pushes a message to the specified `Channel'.
push(Channel, Message) when is_list(Channel) ->
    gen_server:call({global, ?MODULE}, {push, Channel, Message}).

-spec now(channel()) ->    non_neg_integer().
%% @doc Retrieves the current time for the server managing `Channel'.
now(Channel) when is_list(Channel) ->
    gen_server:call({global, ?MODULE}, {now, Channel}).
    
