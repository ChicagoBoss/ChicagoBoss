-module(boss_db_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {adapter, connection, depth = 0}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_db}, ?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_db_adapter_mock),
    {ok, Conn} = Adapter:start(Options),
    {ok, #state{adapter = Adapter, connection = Conn}}.

handle_call({find, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:find(Conn, Key), State};

handle_call({find, Type, Conditions}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:find(Conn, Type, Conditions), State};

handle_call({find, Type, Conditions, Max}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:find(Conn, Type, Conditions, Max), State};

handle_call({find, Type, Conditions, Max, Skip}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:find(Conn, Type, Conditions, Max, Skip), State};

handle_call({find, Type, Conditions, Max, Skip, Sort}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:find(Conn, Type, Conditions, Max, Skip, Sort), State};

handle_call({find, Type, Conditions, Max, Skip, Sort, SortOrder}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:find(Conn, Type, Conditions, Max, Skip, Sort, SortOrder), State};

handle_call({count, Type}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:count(Conn, Type), State};

handle_call({count, Type, Conditions}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:count(Conn, Type, Conditions), State};

handle_call({counter, Counter}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:counter(Conn, Counter), State};

handle_call({incr, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:incr(Conn, Key), State};

handle_call({incr, Key, Count}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:incr(Conn, Key, Count), State};

handle_call({delete, Id}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:delete(Conn, Id), State};

handle_call({save_record, Record}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
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
    Adapter:stop(Conn).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
