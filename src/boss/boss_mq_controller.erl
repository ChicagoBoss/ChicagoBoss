-module(boss_mq_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {adapter, connection}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({global, boss_mq}, ?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_mq_adapter_tinymq),
    {ok, Conn} = Adapter:start(Options),
    {ok, #state{adapter = Adapter, connection = Conn}}.

handle_call({pull, Channel, Timestamp, Subscriber}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:pull(Conn, Channel, Timestamp, Subscriber), State};

handle_call({poll, Channel, Timestamp}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:poll(Conn, Channel, Timestamp), State};

handle_call({push, Channel, Message}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:push(Conn, Channel, Message), State};

handle_call({now, Channel}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:now(Conn, Channel), State}.


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
