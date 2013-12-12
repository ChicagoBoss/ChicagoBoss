-module(boss_session_controller).

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
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_session_adapter_mock),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    {ok, Conn} = Adapter:start(Options),
    {ok, #state{adapter = Adapter, connection = Conn }}.

handle_call({new_session, Cookie}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    NewSessionID = case Adapter:session_exists(Conn, Cookie) of
        true ->
            Cookie;
        false ->
            SessionID = generate_session_id(),
            Adapter:create_session(Conn, SessionID, []),
            SessionID
    end,
    {reply, NewSessionID, State};

handle_call({get_session_data, Sid}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:lookup_session(Conn, Sid), State};

handle_call({get_session_data, Sid, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:lookup_session_value(Conn, Sid, Key), State};

handle_call({set_session_data, Sid, Key, Value}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:set_session_value(Conn, Sid, Key, Value), State};

handle_call({delete_session, SessionID}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:delete_session(Conn, SessionID), State};

handle_call({remove_session_data, Sid, Key}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:delete_session_value(Conn, Sid, Key), State}.

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

generate_session_id() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:hash(sha,Data)),
    lists:flatten(list_to_hex(Sha_list)).
%% Convert Integer from the SHA to Hex
list_to_hex(L)->
       lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 -> 
       [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
