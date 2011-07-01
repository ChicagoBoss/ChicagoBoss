-module(boss_db_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        adapter, 
        connection, 
        shards = [],
        model_dict = dict:new(),
        depth = 0}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_db}, ?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_db_adapter_mock),
    {ok, Conn} = Adapter:start(Options),
    {Shards, ModelDict} = lists:foldr(fun(ShardOptions, {ShardAcc, ModelDictAcc}) ->
                case proplists:get_value(db_shard_models, ShardOptions, []) of
                    [] ->
                        {ShardAcc, ModelDictAcc};
                    Models ->
                        ShardAdapter = proplists:get_value(db_adapter, ShardOptions, Adapter),
                        {ok, ShardConn} = ShardAdapter:start(ShardOptions ++ Options),
                        Index = erlang:length(ShardAcc),
                        NewDict = lists:foldr(fun(ModelAtom, Dict) ->
                                    dict:store(ModelAtom, Index, Dict)
                            end, ModelDictAcc, Models),
                        {[{ShardAdapter, ShardConn}|ShardAcc], NewDict}
                end
        end, {[], dict:new()}, proplists:get_value(shards, Options, [])),
    {ok, #state{adapter = Adapter, connection = Conn, shards = Shards, model_dict = ModelDict}}.

handle_call({find, Key}, _From, State) ->
    {Adapter, Conn} = db_for_key(Key, State),
    {reply, Adapter:find(Conn, Key), State};

handle_call({find, Type, Conditions}, _From, State) ->
    {Adapter, Conn} = db_for_type(Type, State),
    {reply, Adapter:find(Conn, Type, Conditions), State};

handle_call({find, Type, Conditions, Max}, _From, State) ->
    {Adapter, Conn} = db_for_type(Type, State),
    {reply, Adapter:find(Conn, Type, Conditions, Max), State};

handle_call({find, Type, Conditions, Max, Skip}, _From, State) ->
    {Adapter, Conn} = db_for_type(Type, State),
    {reply, Adapter:find(Conn, Type, Conditions, Max, Skip), State};

handle_call({find, Type, Conditions, Max, Skip, Sort}, _From, State) ->
    {Adapter, Conn} = db_for_type(Type, State),
    {reply, Adapter:find(Conn, Type, Conditions, Max, Skip, Sort), State};

handle_call({find, Type, Conditions, Max, Skip, Sort, SortOrder}, _From, State) ->
    {Adapter, Conn} = db_for_type(Type, State),
    {reply, Adapter:find(Conn, Type, Conditions, Max, Skip, Sort, SortOrder), State};

handle_call({count, Type}, _From, State) ->
    {Adapter, Conn} = db_for_type(Type, State),
    {reply, Adapter:count(Conn, Type), State};

handle_call({count, Type, Conditions}, _From, State) ->
    {Adapter, Conn} = db_for_type(Type, State),
    {reply, Adapter:count(Conn, Type, Conditions), State};

handle_call({counter, Counter}, _From, State) ->
    {Adapter, Conn} = db_for_counter(Counter, State),
    {reply, Adapter:counter(Conn, Counter), State};

handle_call({incr, Key}, _From, State) ->
    {Adapter, Conn} = db_for_counter(Key, State),
    {reply, Adapter:incr(Conn, Key), State};

handle_call({incr, Key, Count}, _From, State) ->
    {Adapter, Conn} = db_for_counter(Key, State),
    {reply, Adapter:incr(Conn, Key, Count), State};

handle_call({delete, Id}, _From, State) ->
    {Adapter, Conn} = db_for_key(Id, State),
    {reply, Adapter:delete(Conn, Id), State};

handle_call({save_record, Record}, _From, State) ->
    {Adapter, Conn} = db_for_record(Record, State),
    {reply, Adapter:save_record(Conn, Record), State};

handle_call(push, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Depth = State#state.depth,
    {reply, Adapter:push(Conn, Depth), State#state{depth = Depth + 1}};

handle_call(pop, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Depth = State#state.depth,
    {reply, Adapter:pop(Conn, Depth), State#state{depth = Depth - 1}};

handle_call(depth, _From, State) ->
    {reply, State#state.depth, State};

handle_call(dump, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:dump(Conn), State};

handle_call({execute, Commands}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:execute(Conn, Commands), State};

handle_call({transaction, TransactionFun}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:transaction(Conn, TransactionFun), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Adapter:stop(Conn),
    lists:map(fun({A, C}) ->
                A:stop(C)
        end, State#state.shards).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

db_for_counter(_Counter, State) ->
    {State#state.adapter, State#state.connection}.

db_for_record(Record, State) ->
    db_for_type(element(1, Record), State).

db_for_key(Key, State) ->
    db_for_type(infer_type_from_id(Key), State).

db_for_type(Type, State) ->
    case dict:find(Type, State#state.model_dict) of
        {ok, Index} ->
            lists:nth(Index + 1, State#state.shards);
        _ ->
            {State#state.adapter, State#state.connection}
    end.

infer_type_from_id(Id) when is_binary(Id) ->
    infer_type_from_id(binary_to_list(Id));
infer_type_from_id(Id) when is_list(Id) ->
    list_to_atom(hd(string:tokens(Id, "-"))).
