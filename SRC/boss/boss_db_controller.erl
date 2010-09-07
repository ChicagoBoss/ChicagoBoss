-module(boss_db_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {driver}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_db}, ?MODULE, Args, []).

init(Options) ->
    DBDriver = proplists:get_value(driver, Options, boss_db_driver_tyrant),
    ok = DBDriver:start(),
    {ok, #state{driver = DBDriver}}.

handle_call({find, Key}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:find(Key), State};

handle_call({find, Type, Conditions}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:find(Type, Conditions), State};

handle_call({find, Type, Conditions, Max}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:find(Type, Conditions, Max), State};

handle_call({find, Type, Conditions, Max, Skip}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:find(Type, Conditions, Max, Skip), State};

handle_call({find, Type, Conditions, Max, Skip, Sort}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:find(Type, Conditions, Max, Skip, Sort), State};

handle_call({find, Type, Conditions, Max, Skip, Sort, SortOrder}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:find(Type, Conditions, Max, Skip, Sort, SortOrder), State};

handle_call({count, Type}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:count(Type), State};

handle_call({count, Type, Conditions}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:count(Type, Conditions), State};

handle_call({counter, Counter}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:counter(Counter), State};

handle_call({incr, Key}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:incr(Key), State};

handle_call({incr, Key, Count}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:incr(Key, Count), State};

handle_call({delete, Id}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:delete(Id), State};

handle_call({save_record, Record}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:save_record(Record), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Driver = State#state.driver,
    Driver:stop().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
