-module(boss_cache_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        adapter,
        connection
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_cache}, ?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_cache_adapter_memcached_bin),
    {ok, Conn} = Adapter:start(Options),
    {ok, #state{ adapter = Adapter, connection = Conn }}.

handle_call({get, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:get(Conn, Key), State};
handle_call({set, Key, Value, TTL}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:set(Conn, Key, Value, TTL), State};
handle_call({delete, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:delete(Conn, Key), State}.

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
