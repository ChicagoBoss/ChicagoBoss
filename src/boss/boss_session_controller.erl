-module(boss_session_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        adapter, 
        session_dict = dict:new(),
        ttl_tree = gb_trees:empty(),
        exp_time,
        connection
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_session}, ?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_session_adapter_ets),
    ExpTime = proplists:get_value(session_exp_time, Options, 1440),
    {ok, Conn} = Adapter:start(Options),
    {ok, #state{adapter = Adapter, connection = Conn, exp_time = ExpTime}}.

handle_call({new_session, Cookie}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    SessionID = Adapter:new_session(Conn, Cookie),
    NewState = case Adapter:needs_expiration() of
        false ->
            State;
        true ->
            NowSeconds = now_seconds(),
            NewTree = case dict:find(SessionID, State#state.session_dict) of
                {ok, Val} ->
                    boss_pq:move_value(Val, NowSeconds + State#state.exp_time, SessionID, State#state.ttl_tree);
                _ ->
                    boss_pq:insert_value(NowSeconds + State#state.exp_time, SessionID, State#state.ttl_tree)
            end,
            prune_expired_sessions(State#state{
                ttl_tree = NewTree,
                session_dict = dict:store(SessionID,
                    NowSeconds + State#state.exp_time,
                    State#state.session_dict)
            }, NowSeconds)
    end,

    {reply, SessionID, NewState};

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

handle_call({delete_session, SessionID}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    NewState = case Adapter:needs_expiration() of
        false -> 
            State;
        true ->
            NewTree = case dict:find(SessionID, State#state.session_dict) of
                {ok, Val} ->
                    boss_pq:delete_value(Val, SessionID, State#state.ttl_tree);
                _ ->
                    State#state.ttl_tree
            end,

            State#state{
                ttl_tree = NewTree,
                session_dict = dict:erase(SessionID, State#state.session_dict)
            }
    end,
    {reply, Adapter:delete_session(Conn, SessionID), NewState};

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

now_seconds() ->
    {A, B, _} = erlang:now(),
    A * 1000 * 1000 + B.

prune_expired_sessions(#state{ ttl_tree = Tree, session_dict = Dict, adapter = A, connection = C } = State, NowSeconds) ->
    {NewDict, NewTree} = boss_pq:prune(fun(SessionID, DictAcc) ->
                A:delete_session(C, SessionID),
                dict:erase(SessionID, DictAcc)
        end, Dict, Tree, NowSeconds),
    State#state{ ttl_tree = NewTree, session_dict = NewDict }.
