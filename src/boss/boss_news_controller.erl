-module(boss_news_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        watch_dict = dict:new(),
        ttl_tree = gb_trees:empty(),

        set_watchers = dict:new(), 
        id_watchers = dict:new(),

        set_attr_watchers = dict:new(),
        id_attr_watchers = dict:new(),
        watch_counter = 0}).

-record(watch, {
        watch_list = [],
        callback,
        user_info,
        exp_time,
        ttl}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({global, boss_news}, ?MODULE, Args, []).

init(_Options) ->
    {ok, #state{}}.

handle_call(reset, _From, _State) ->
    {reply, ok, #state{}};
handle_call(dump, _From, State0) ->
    State = prune_expired_entries(State0),
    {reply, State, State};
handle_call({watch, TopicString, CallBack, UserInfo, TTL}, From, State0) ->
    WatchId = State0#state.watch_counter,
    {reply, RetVal, State} = handle_call({set_watch, WatchId, TopicString, CallBack, UserInfo, TTL}, From, State0),
    case RetVal of
        ok ->
            {reply, {ok, WatchId}, State#state{ watch_counter = WatchId + 1 }};
        Other ->
            {reply, Other, State0}
    end;
handle_call({set_watch, WatchId, TopicString, CallBack, UserInfo, TTL}, From, State0) ->
    {reply, _, State} = handle_call({cancel_watch, WatchId}, From, State0),
    ExpTime = future_time(TTL),
    {RetVal, NewState, WatchList} = lists:foldr(fun
            (SingleTopic, {ok, StateAcc, WatchListAcc}) ->
                case re:split(SingleTopic, "\\.", [{return, list}]) of
                    [Id, Attr] ->
                        [Module, IdNum] = re:split(Id, "-", [{return, list}]),
                        {NewState1, WatchInfo} = case IdNum of
                            "*" ->
                                SetAttrWatchers = case dict:find(Module, StateAcc#state.set_attr_watchers) of
                                    {ok, Val} -> Val;
                                    _ -> []
                                end,
                                {StateAcc#state{
                                        set_attr_watchers = dict:store(Module, [WatchId|SetAttrWatchers], StateAcc#state.set_attr_watchers)
                                    }, {set_attr, Module, Attr}};
                            _ ->
                                IdAttrWatchers = case dict:find(Id, StateAcc#state.id_attr_watchers) of
                                    {ok, Val} -> Val;
                                    _ -> []
                                end,
                                {StateAcc#state{
                                        id_attr_watchers = dict:store(Id, [WatchId|IdAttrWatchers], StateAcc#state.id_attr_watchers)
                                    }, {id_attr, Id, Attr}}
                        end,
                        {ok, NewState1, [WatchInfo|WatchListAcc]};
                    _ -> 
                        case re:split(SingleTopic, "-", [{return, list}]) of
                            [_Module, _IdNum] ->
                                IdWatchers = case dict:find(SingleTopic, State#state.id_watchers) of
                                    {ok, Val} -> Val;
                                    _ -> []
                                end,
                                {ok, StateAcc#state{
                                        id_watchers = dict:store(SingleTopic, [WatchId|IdWatchers], StateAcc#state.id_watchers)
                                    }, [{id, SingleTopic}|WatchListAcc]};
                            [_PluralModel] ->
                                SetWatchers = case dict:find(SingleTopic, StateAcc#state.set_watchers) of
                                    {ok, Val} -> Val;
                                    _ -> []
                                end,
                                {ok, StateAcc#state{
                                        set_watchers = dict:store(SingleTopic, [WatchId|SetWatchers], StateAcc#state.set_watchers)
                                    }, [{set, SingleTopic}|WatchListAcc]}
                        end
                end;
            (_, Error) ->
                Error
        end, {ok, State, []}, re:split(TopicString, ", +", [{return, list}])),
    case RetVal of
        ok -> {reply, RetVal, NewState#state{ 
                    watch_dict = dict:store(WatchId, 
                        #watch{ 
                            watch_list = WatchList, 
                            callback = CallBack, 
                            user_info = UserInfo, 
                            exp_time = ExpTime, 
                            ttl = TTL}, NewState#state.watch_dict),
                    ttl_tree = boss_pq:insert_value(ExpTime, WatchId, NewState#state.ttl_tree)
                }};
        Error -> {reply, Error, State}
    end;
handle_call({cancel_watch, WatchId}, _From, State) ->
    {RetVal, NewState} = case dict:find(WatchId, State#state.watch_dict) of
        {ok, #watch{ exp_time = ExpTime }} ->
            NewTree = boss_pq:move_value(ExpTime, 0, WatchId, State#state.ttl_tree),
            {ok, State#state{ ttl_tree = NewTree }};
        _ ->
            {{error, not_found}, State}
    end,
    {reply, RetVal, prune_expired_entries(NewState)};
handle_call({extend_watch, WatchId}, _From, State0) ->
    State = prune_expired_entries(State0),
    {RetVal, NewState} = case dict:find(WatchId, State#state.watch_dict) of
        {ok, #watch{ exp_time = ExpTime, ttl = TTL } = Watch} ->
            NewExpTime = future_time(TTL),
            NewTree = boss_pq:move_value(ExpTime, NewExpTime, WatchId, State#state.ttl_tree),
            {ok, State#state{ ttl_tree = NewTree, 
                    watch_dict = dict:store(WatchId, Watch#watch{ exp_time = NewExpTime }, State#state.watch_dict) }};
        _ ->
            {{error, not_found}, State}
    end,
    {reply, RetVal, NewState};
handle_call({created, Id, Attrs}, _From, State0) ->
    State = prune_expired_entries(State0),
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    PluralModel = inflector:pluralize(Module),
    {RetVal, State1} = case dict:find(PluralModel, State#state.set_watchers) of
        {ok, SetWatchers} -> 
            Record = activate_record(Id, Attrs),
            NewState = lists:foldr(fun(WatchId, Acc0) ->
                        #watch{ watch_list = WatchList, 
                            callback = CallBack, 
                            user_info = UserInfo } = dict:fetch(WatchId, State#state.watch_dict),
                        lists:foldr(fun
                                ({set, TopicString}, Acc1) when TopicString =:= PluralModel ->
                                    execute_callback(CallBack, created, Record, UserInfo, WatchId, Acc1);
                                (_, Acc1) ->
                                    Acc1
                            end, Acc0, WatchList)
                end, State, SetWatchers),
            {ok, NewState};
        _ -> {ok, State}
    end,
    {reply, RetVal, State1};
handle_call({deleted, Id, OldAttrs}, _From, State0) ->
    State = prune_expired_entries(State0),
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    PluralModel = inflector:pluralize(Module),
    {RetVal, State1} = case dict:find(PluralModel, State#state.set_watchers) of
        {ok, SetWatchers} -> 
            Record = activate_record(Id, OldAttrs),
            NewState = lists:foldr(fun(WatchId, Acc0) ->
                        #watch{ watch_list = WatchList, 
                            callback = CallBack, 
                            user_info = UserInfo } = dict:fetch(WatchId, State#state.watch_dict),
                        lists:foldr(fun
                                ({set, TopicString}, Acc1) when TopicString =:= PluralModel ->
                                    execute_callback(CallBack, deleted, Record, UserInfo, WatchId, Acc1);
                                ({id, TopicString}, Acc1) when TopicString =:= Id ->
                                    execute_callback(CallBack, deleted, Record, UserInfo, WatchId, Acc1);
                                (_, Acc1) ->
                                    Acc1
                            end, Acc0, WatchList)
                end, State, SetWatchers),
            {ok, NewState};
        _ -> {ok, State}
    end,
    {reply, RetVal, State1};
handle_call({updated, Id, OldAttrs, NewAttrs}, _From, State0) ->
    State = prune_expired_entries(State0),
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    IdWatchers = case dict:find(Id, State#state.id_attr_watchers) of
        {ok, Val} -> Val;
        _ -> []
    end,
    WildcardWatchers = case dict:find(Module, State#state.set_attr_watchers) of
        {ok, Val1} -> Val1;
        _ -> []
    end,
    AllWatchers = IdWatchers ++ WildcardWatchers,
    OldRecord = activate_record(Id, OldAttrs),
    NewRecord = activate_record(Id, NewAttrs),
    NewState = lists:foldr(fun
            ({Key, OldVal}, Acc0) ->
                KeyString = atom_to_list(Key),
                case NewRecord:Key() of
                    OldVal -> Acc0;
                    NewVal -> 
                        lists:foldr(fun(WatchId, Acc1) ->
                                    #watch{ watch_list = WatchList, 
                                        callback = CallBack, 
                                        user_info = UserInfo } = dict:fetch(WatchId, State#state.watch_dict),
                                    lists:foldr(fun
                                            ({id_attr, ThisId, Attr}, Acc2) when ThisId =:= Id, Attr =:= KeyString ->
                                                execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo, WatchId, Acc2);
                                            ({id_attr, ThisId, "*"}, Acc2) when ThisId =:= Id ->
                                                execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo, WatchId, Acc2);
                                            ({set_attr, ThisModule, Attr}, Acc2) when ThisModule =:= Module, Attr =:= KeyString ->
                                                execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo, WatchId, Acc2);
                                            ({set_attr, ThisModule, "*"}, Acc2) when ThisModule =:= Module ->
                                                execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo, WatchId, Acc2);
                                            (_, Acc2) -> Acc2
                                        end, Acc1, WatchList)
                            end, Acc0, AllWatchers)
                end
        end, State, OldRecord:attributes()),
    {reply, ok, NewState}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.


future_time(TTL) ->
    {MegaSecs, Secs, _} = erlang:now(),
    MegaSecs * 1000 * 1000 + Secs + TTL.

activate_record(Id, Attrs) ->
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    Type = list_to_atom(Module),
    DummyRecord = boss_record_lib:dummy_record(Type),
    apply(Type, new, lists:map(fun
                (id) -> Id;
                (Key) -> proplists:get_value(Key, Attrs)
            end, DummyRecord:attribute_names())).

prune_expired_entries(#state{ ttl_tree = Tree } = State) ->
    Now = future_time(0),
    {NewState, NewTree} = boss_pq:prune(fun(WatchId, StateAcc) ->
                #watch{ watch_list = WatchList } = dict:fetch(WatchId, StateAcc#state.watch_dict),
                NewState = lists:foldr(fun
                        ({id, TopicString}, Acc) ->
                            NewDict = case dict:fetch(TopicString, StateAcc#state.id_watchers) of
                                [WatchId] ->
                                    dict:erase(TopicString, StateAcc#state.id_watchers);
                                Watchers ->
                                    dict:store(TopicString, lists:delete(WatchId, Watchers), StateAcc#state.id_watchers)
                            end,
                            Acc#state{ id_watchers = NewDict };
                        ({set, TopicString}, Acc) ->
                            NewDict = case dict:fetch(TopicString, StateAcc#state.set_watchers) of
                                [WatchId] ->
                                    dict:erase(TopicString, StateAcc#state.set_watchers);
                                Watchers ->
                                    dict:store(TopicString, lists:delete(WatchId, Watchers), StateAcc#state.set_watchers)
                            end,
                            Acc#state{ set_watchers = NewDict };
                        ({id_attr, Id, _Attr}, Acc) ->
                            NewDict = case dict:fetch(Id, StateAcc#state.id_attr_watchers) of
                                [WatchId] ->
                                    dict:erase(Id, StateAcc#state.id_attr_watchers);
                                Watchers ->
                                    dict:store(Id, lists:delete(WatchId, Watchers), StateAcc#state.id_attr_watchers)
                            end,
                            Acc#state{ id_attr_watchers = NewDict };
                        ({set_attr, Module, _Attr}, Acc) ->
                            NewDict = case dict:fetch(Module, StateAcc#state.set_attr_watchers) of
                                [WatchId] ->
                                    dict:erase(Module, StateAcc#state.set_attr_watchers);
                                Watchers ->
                                    dict:store(Module, lists:delete(WatchId, Watchers), StateAcc#state.set_attr_watchers)
                            end,
                            Acc#state{ set_attr_watchers = NewDict };
                        (_, Acc) ->
                            Acc
                    end, StateAcc, WatchList),
                NewState#state{ watch_dict = dict:erase(WatchId, StateAcc#state.watch_dict) }
        end, State, Tree, Now),
    NewState#state{ ttl_tree = NewTree }.

execute_callback(Fun, Event, EventInfo, UserInfo, WatchId, State) when is_function(Fun) ->
    Result = case proplists:get_value(arity, erlang:fun_info(Fun)) of
        2 ->
            Fun(Event, EventInfo);
        3 ->
            Fun(Event, EventInfo, UserInfo)
    end,
    case Result of
        {ok, cancel_watch} -> 
            {reply, _, NewState} = handle_call({cancel_watch, WatchId}, undefined, State),
            NewState;
        {ok, extend_watch} -> 
            {reply, _, NewState} = handle_call({extend_watch, WatchId}, undefined, State),
            NewState;
        _ ->
            State
    end.
