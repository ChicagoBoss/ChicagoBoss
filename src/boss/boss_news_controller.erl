-module(boss_news_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        watch_dict = dict:new(),
        ttl_tree = gb_trees:empty(),

        set_watchers = dict:new(), 
        wildcard_watchers = dict:new(),
        id_watchers = dict:new(),
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
    gen_server:start_link({local, boss_news}, ?MODULE, Args, []).

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
    Models = boss_files:model_list(),
    ExpTime = future_time(TTL),
    {RetVal, NewState, WatchList} = lists:foldr(fun
            (SingleTopic, {ok, StateAcc, WatchListAcc}) ->
                case re:split(SingleTopic, "\\.", [{return, list}]) of
                    [Id, Attr] ->
                        [Module, IdNum] = re:split(Id, "-", [{return, list}]),
                        case lists:member(Module, Models) of
                            false ->
                                {{error, model_not_found}, StateAcc, WatchListAcc};
                            true ->
                                {NewState1, WatchInfo} = case IdNum of
                                    "*" ->
                                        WildcardWatchers = case dict:find(Module, StateAcc#state.wildcard_watchers) of
                                            {ok, Val} -> Val;
                                            _ -> []
                                        end,
                                        {StateAcc#state{
                                                wildcard_watchers = dict:store(Module, [WatchId|WildcardWatchers], StateAcc#state.wildcard_watchers)
                                            }, {module, Module, Attr}};
                                    _ ->
                                        IdWatchers = case dict:find(Id, StateAcc#state.id_watchers) of
                                            {ok, Val} -> Val;
                                            _ -> []
                                        end,
                                        {StateAcc#state{
                                            id_watchers = dict:store(Id, [WatchId|IdWatchers], StateAcc#state.id_watchers)
                                        }, {id, Id, Attr}}
                                end,
                                {ok, NewState1, [WatchInfo|WatchListAcc]}
                        end;
                    _ -> 
                        case lists:member(inflector:singularize(SingleTopic), Models) of
                            false ->
                                {{error, model_not_found}, StateAcc, WatchListAcc};
                            true ->
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
                    ttl_tree = insert_watch(ExpTime, WatchId, NewState#state.ttl_tree)
                }};
        Error -> {reply, Error, State}
    end;
handle_call({cancel_watch, WatchId}, _From, State) ->
    {RetVal, NewState} = case dict:find(WatchId, State#state.watch_dict) of
        {ok, #watch{ exp_time = ExpTime }} ->
            NewTree = case gb_trees:lookup(ExpTime, State#state.ttl_tree) of
                {value, [WatchId]} ->
                    insert_watch(0, WatchId, gb_trees:delete(ExpTime, State#state.ttl_tree));
                {value, Watches} ->
                    insert_watch(0, WatchId, gb_trees:update(ExpTime, lists:delete(WatchId, Watches), State#state.ttl_tree))
            end,
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
            NewTree = case gb_trees:lookup(ExpTime, State#state.ttl_tree) of
                {value, [WatchId]} ->
                    insert_watch(NewExpTime, WatchId, gb_trees:delete(ExpTime, State#state.ttl_tree));
                {value, Watches} ->
                    insert_watch(NewExpTime, WatchId, gb_trees:enter(ExpTime, lists:delete(WatchId, Watches), State#state.ttl_tree))
            end,
            {ok, State#state{ ttl_tree = NewTree, 
                    watch_dict = dict:store(WatchId, Watch#watch{ exp_time = NewExpTime }, State#state.watch_dict) }};
        _ ->
            {{error, not_found}, State}
    end,
    {reply, RetVal, NewState};
handle_call({created, Id, Attrs}, _From, State0) ->
    State = prune_expired_entries(State0),
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    Models = boss_files:model_list(),
    RetVal = case lists:member(Module, Models) of
        false ->
            {error, model_not_found};
        true ->
            PluralModel = inflector:pluralize(Module),
            case dict:find(PluralModel, State#state.set_watchers) of
                {ok, SetWatchers} -> 
                    Record = activate_record(Id, Attrs),
                    lists:map(fun(WatchId) ->
                                #watch{ watch_list = WatchList, 
                                    callback = CallBack, 
                                    user_info = UserInfo } = dict:fetch(WatchId, State#state.watch_dict),
                                lists:map(fun
                                        ({set, TopicString}) when TopicString =:= PluralModel ->
                                            execute_callback(CallBack, event, Record, UserInfo);
                                        (_) ->
                                            ok
                                    end, WatchList)
                        end, SetWatchers),
                    ok;
                _ -> ok
            end
    end,
    {reply, RetVal, State};
handle_call({deleted, Id, OldAttrs}, _From, State0) ->
    State = prune_expired_entries(State0),
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    Models = boss_files:model_list(),
    RetVal = case lists:member(Module, Models) of
        false ->
            {error, model_not_found};
        true ->
            PluralModel = inflector:pluralize(Module),
            case dict:find(PluralModel, State#state.set_watchers) of
                {ok, SetWatchers} -> 
                    Record = activate_record(Id, OldAttrs),
                    lists:map(fun(WatchId) ->
                                #watch{ watch_list = WatchList, 
                                    callback = CallBack, 
                                    user_info = UserInfo } = dict:fetch(WatchId, State#state.watch_dict),
                                lists:map(fun
                                        ({set, TopicString}) when TopicString =:= PluralModel ->
                                            execute_callback(CallBack, deleted, Record, UserInfo);
                                        (_) ->
                                            ok
                                    end, WatchList)
                        end, SetWatchers),
                    ok;
                _ -> ok
            end
    end,
    {reply, RetVal, State};
handle_call({updated, Id, OldAttrs, NewAttrs}, _From, State0) ->
    State = prune_expired_entries(State0),
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    Models = boss_files:model_list(),
    RetVal = case lists:member(Module, Models) of
        false ->
            {error, model_not_found};
        true ->
            IdWatchers = case dict:find(Id, State#state.id_watchers) of
                {ok, Val} -> Val;
                _ -> []
            end,
            WildcardWatchers = case dict:find(Module, State#state.wildcard_watchers) of
                {ok, Val1} -> Val1;
                _ -> []
            end,
            AllWatchers = IdWatchers ++ WildcardWatchers,
            OldRecord = activate_record(Id, OldAttrs),
            NewRecord = activate_record(Id, NewAttrs),
            lists:map(fun
                    ({Key, OldVal}) ->
                        KeyString = atom_to_list(Key),
                        case NewRecord:Key() of
                            OldVal -> ok;
                            NewVal -> 
                                lists:map(fun(WatchId) ->
                                            #watch{ watch_list = WatchList, 
                                                callback = CallBack, 
                                                user_info = UserInfo } = dict:fetch(WatchId, State#state.watch_dict),
                                            lists:map(fun
                                                    ({id, ThisId, Attr}) when ThisId =:= Id, Attr =:= KeyString ->
                                                        execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo);
                                                    ({id, ThisId, "*"}) when ThisId =:= Id ->
                                                        execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo);
                                                    ({module, ThisModule, Attr}) when ThisModule =:= Module, Attr =:= KeyString ->
                                                        execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo);
                                                    ({module, ThisModule, "*"}) when ThisModule =:= Module ->
                                                        execute_callback(CallBack, updated, {NewRecord, Key, OldVal, NewVal}, UserInfo);
                                                    (_) -> ok
                                                end, WatchList)
                                    end, AllWatchers)
                        end
                end, OldRecord:attributes()),
            ok
    end,
    {reply, RetVal, State}.

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

insert_watch(FutureTime, WatchId, Tree) ->
    NewVal = case gb_trees:lookup(FutureTime, Tree) of
        none -> [WatchId];
        {value, Watches} -> [WatchId|Watches]
    end,
    gb_trees:enter(FutureTime, NewVal, Tree).

activate_record(Id, Attrs) ->
    [Module | _IdNum] = re:split(Id, "-", [{return, list}]),
    Type = list_to_atom(Module),
    DummyRecord = apply(Type, new, lists:seq(1, proplists:get_value(new, Type:module_info(exports)))),
    apply(Type, new, lists:map(fun
                (id) -> Id;
                (Key) -> proplists:get_value(Key, Attrs)
            end, DummyRecord:attribute_names())).

prune_expired_entries(#state{ ttl_tree = {Size, TreeNode} } = State) ->
    Now = future_time(0),
    {NewState, NewTree, NumDeleted} = prune_expired_nodes(fun(WatchId, StateAcc) ->
                #watch{ watch_list = WatchList } = dict:fetch(WatchId, StateAcc#state.watch_dict),
                NewState = lists:foldr(fun
                        ({set, TopicString}, Acc) ->
                            NewDict = case dict:fetch(TopicString, StateAcc#state.set_watchers) of
                                [WatchId] ->
                                    dict:erase(TopicString, StateAcc#state.set_watchers);
                                Watchers ->
                                    dict:store(TopicString, lists:delete(WatchId, Watchers), StateAcc#state.set_watchers)
                            end,
                            Acc#state{ set_watchers = NewDict };
                        ({id, Id, _Attr}, Acc) ->
                            NewDict = case dict:fetch(Id, StateAcc#state.id_watchers) of
                                [WatchId] ->
                                    dict:erase(Id, StateAcc#state.id_watchers);
                                Watchers ->
                                    dict:store(Id, lists:delete(WatchId, Watchers), StateAcc#state.id_watchers)
                            end,
                            Acc#state{ id_watchers = NewDict };
                        ({module, Module, _Attr}, Acc) ->
                            NewDict = case dict:fetch(Module, StateAcc#state.wildcard_watchers) of
                                [WatchId] ->
                                    dict:erase(Module, StateAcc#state.wildcard_watchers);
                                Watchers ->
                                    dict:store(Module, lists:delete(WatchId, Watchers), StateAcc#state.wildcard_watchers)
                            end,
                            Acc#state{ wildcard_watchers = NewDict };
                        (_, Acc) ->
                            Acc
                    end, StateAcc, WatchList),
                NewState#state{ watch_dict = dict:erase(WatchId, StateAcc#state.watch_dict) }
        end, State, TreeNode, Now),
    NewState#state{ ttl_tree = {Size - NumDeleted, NewTree} }.

prune_expired_nodes(Function, Acc, {K, V, S, L}, Now) when K > Now ->
    {Acc1, Tree1, NumDeleted} = prune_expired_nodes(Function, Acc, S, Now),
    {Acc1, {K, V, Tree1, L}, NumDeleted};
prune_expired_nodes(Function, Acc, {K, V, S, L}, Now) when K =< Now ->
    Acc1 = lists:foldr(Function, Acc, V),
    {Acc2, _, NumDeleted_S} = prune_expired_nodes(Function, Acc1, S, Now),
    {Acc3, Tree3, NumDeleted_L} = prune_expired_nodes(Function, Acc2, L, Now),
    {Acc3, Tree3, NumDeleted_S + NumDeleted_L + 1};
prune_expired_nodes(_Function, Acc, nil, _Now) ->
    {Acc, nil, 0}.

execute_callback(Fun, Event, EventInfo, UserInfo) when is_function(Fun) ->
    Fun(Event, EventInfo, UserInfo).
