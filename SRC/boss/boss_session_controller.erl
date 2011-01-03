-module(boss_session_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {adapter, connection, depth = 0}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_session}, ?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_session_adapter_ets),
    {ok, Conn} = Adapter:start(Options),
    {ok, #state{adapter = Adapter, connection = Conn}}.

handle_call({new_session, Cookie}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:new_session(Conn, Cookie), State};

handle_call({get_session_data, Sid}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:get_session_data(Conn, Sid), State};

handle_call({get_session_data, Sid, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:get_session_data(Conn, Sid, Key), State};

handle_call({set_session_data, Sid, Key, Value}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:set_session_data(Conn, Sid, Key, Value), State};

handle_call({delete_session, Sid}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:delete_session(Conn, Sid), State};

handle_call({remove_session_data, Sid, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:remove_session_data(Conn, Sid, Key), State}.

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
