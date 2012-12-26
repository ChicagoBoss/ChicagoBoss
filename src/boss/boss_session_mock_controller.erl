-module(boss_session_mock_controller).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(boss_session, {sid, data}).

-record(state, {
        table, 
        session_dict = dict:new(),
        ttl_tree = gb_trees:empty(),
        exp_time
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({global, boss_session_mock}, ?MODULE, Args, []).

init(Options) ->
    TableId = ets:new(?MODULE,[set,named_table,{keypos, 2}]),
    {ok, #state{ table = TableId, exp_time = proplists:get_value(session_exp_time, Options, 1440) }}.

handle_call({session_exists, SessionID}, _From, State) ->
    Exists = dict:is_key(SessionID, State#state.session_dict),
    NewState = case Exists of
        true ->
            NowSeconds = now_seconds(),
            Val = dict:fetch(SessionID, State#state.session_dict),
            State#state{ 
                ttl_tree = tiny_pq:move_value(Val, NowSeconds + State#state.exp_time, SessionID, State#state.ttl_tree),
                session_dict = dict:store(SessionID,
                    NowSeconds + State#state.exp_time,
                    State#state.session_dict)};
        false ->
            State
    end,
    {reply, Exists, prune_expired_sessions(NewState, now_seconds())};
handle_call({create_session, SessionID, Data}, _From, State) ->
    NowSeconds = now_seconds(),
    ets:insert(State#state.table, #boss_session{sid=SessionID, data=Data}),
    NewState = State#state{ 
        ttl_tree = tiny_pq:insert_value(NowSeconds + State#state.exp_time, SessionID, State#state.ttl_tree),
        session_dict = dict:store(SessionID, NowSeconds + State#state.exp_time, State#state.session_dict)
    },
    {reply, ok, NewState};
handle_call({lookup_session, SessionID}, _From, State) ->
    Data = case ets:lookup(State#state.table, SessionID) of
        [S] -> S#boss_session.data;
        [] -> []
    end,
    {reply, Data, State};
handle_call({lookup_session_value, SessionID, Key}, _From, State) ->
    Data = case ets:lookup(State#state.table, SessionID) of
        [S] -> S#boss_session.data;
        [] -> []
    end,
    {reply, proplists:get_value(Key, Data), State};
handle_call({set_session_value, SessionID, Key, Value}, _From, State) ->
    Data = case ets:lookup(State#state.table, SessionID) of
        [S] -> S#boss_session.data;
        [] -> []
    end,
    Data1 = case proplists:is_defined(Key,Data) of
        true ->
            Rest = proplists:delete(Key,Data),
            [{Key,Value}|Rest];
        false ->
            [{Key,Value}|Data]
    end,
    ets:insert(State#state.table, #boss_session{sid=SessionID,data=Data1}),
    {reply, ok, State};
handle_call({delete_session, SessionID}, _From, State) ->
    ets:delete(State#state.table, SessionID),
    NewState = case dict:find(SessionID, State#state.session_dict) of
        {ok, Val} ->
            State#state{ 
                ttl_tree = tiny_pq:delete_value(Val, SessionID, State#state.ttl_tree),
                session_dict = dict:erase(SessionID, State#state.session_dict)
            };
        _ ->
            State
    end,
    {reply, ok, NewState};
handle_call({delete_session_value, SessionID, Key}, _From, State) ->
    case ets:lookup(?MODULE,SessionID) of
        [S] ->
            Data = S#boss_session.data,
            case proplists:is_defined(Key,Data) of
                true ->
                    Data1 = proplists:delete(Key,Data),
                    ets:insert(State#state.table, #boss_session{ sid=SessionID, data=Data1 });
                false ->
                    ok
            end;
        [] -> 
            ok
    end,
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

prune_expired_sessions(#state{ ttl_tree = Tree, session_dict = Dict, table = TableId } = State, NowSeconds) ->
    {NewDict, NewTree} = tiny_pq:prune_collect_old(fun(SessionID, DictAcc) ->
                ets:delete(TableId, SessionID),
                dict:erase(SessionID, DictAcc)
        end, Dict, Tree, NowSeconds),
    State#state{ ttl_tree = NewTree, session_dict = NewDict }.

now_seconds() ->
    {A, B, _} = erlang:now(),
    A * 1000 * 1000 + B.

