-module(boss_news_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        set_watchers, 
        wildcard_watchers,
        id_watchers}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_news}, ?MODULE, Args, []).

init(_Options) ->
    {ok, #state{set_watchers = dict:new(), wildcard_watchers = dict:new(), id_watchers = dict:new()}}.

handle_call(reset, _From, _State) ->
    {reply, ok, #state{set_watchers = dict:new(), wildcard_watchers = dict:new(),
            id_watchers = dict:new()}};
handle_call({watch, TopicString, CallBack}, _From, State) ->
    [Id, Attr] = re:split(TopicString, "\\.", [{return, list}]),
    [Module, IdNum] = re:split(Id, "-", [{return, list}]),
    Models = boss_files:model_list(),
    {RetVal, NewState} = case lists:member(Module, Models) of
        false ->
            {{error, model_not_found}, State};
        true ->
            NewState1 = case IdNum of
                "*" ->
                    WildcardWatchers = case dict:find(Module, State#state.wildcard_watchers) of
                        {ok, Val} -> Val;
                        _ -> []
                    end,
                    State#state{wildcard_watchers = dict:store(Module,
                            [{Attr, CallBack}|WildcardWatchers], State#state.wildcard_watchers)};
                _ ->
                    IdWatchers = case dict:find(Id, State#state.id_watchers) of
                        {ok, Val} -> Val;
                        _ -> []
                    end,
                    State#state{id_watchers = dict:store(Id, 
                            [{Attr, CallBack}|IdWatchers], State#state.id_watchers)}
            end,
            {ok, NewState1}
    end,
    {reply, RetVal, NewState};
handle_call({watch_set, TopicString, CallBack}, _From, State) ->
    Models = boss_files:model_list(),
    {RetVal, NewState} = case lists:member(inflector:singularize(TopicString), Models) of
        false ->
            {{error, model_not_found}, State};
        true ->
            SetWatchers = case dict:find(TopicString, State#state.set_watchers) of
                {ok, Val} -> Val;
                _ -> []
            end,
            {ok, State#state{set_watchers = dict:store(TopicString,
                        [CallBack|SetWatchers], State#state.set_watchers)}}
    end,
    {reply, RetVal, NewState};
handle_call({created, Id, Attrs}, _From, State) ->
    [Module, _IdNum] = re:split(Id, "-", [{return, list}]),
    Models = boss_files:model_list(),
    RetVal = case lists:member(Module, Models) of
        false ->
            {error, model_not_found};
        true ->
            SetWatchers = case dict:find(inflector:pluralize(Module), State#state.set_watchers) of
                {ok, Val} -> Val;
                _ -> []
            end,
            Record = activate_record(Id, Attrs),
            lists:map(fun(CallBack) -> CallBack(created, Record) end, SetWatchers),
            ok
    end,
    {reply, RetVal, State};
handle_call({deleted, Id, OldAttrs}, _From, State) ->
    [Module, _IdNum] = re:split(Id, "-", [{return, list}]),
    Models = boss_files:model_list(),
    RetVal = case lists:member(Module, Models) of
        false ->
            {error, model_not_found};
        true ->
            SetWatchers = case dict:find(inflector:pluralize(Module), State#state.set_watchers) of
                {ok, Val} -> Val;
                _ -> []
            end,
            Record = activate_record(Id, OldAttrs),
            lists:map(fun(CallBack) -> CallBack(deleted, Record) end, SetWatchers)
    end,
    {reply, RetVal, State};
handle_call({updated, Id, OldAttrs, NewAttrs}, _From, State) ->
    [Module, _IdNum] = re:split(Id, "-", [{return, list}]),
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
            error_logger:error_msg("Handling update: ~p ~p~n", [OldRecord, NewRecord]),
            lists:map(fun
                    ({Key, OldVal}) ->
                        KeyString = atom_to_list(Key),
                        case NewRecord:Key() of
                            OldVal -> ok;
                            NewVal -> 
                                lists:map(fun
                                        ({KeyString1, CallBack}) when KeyString1 =:= KeyString ->
                                            CallBack(NewRecord, Key, OldVal, NewVal);
                                        ({"*", CallBack}) ->
                                            CallBack(NewRecord, Key, OldVal, NewVal);
                                        (_) -> ok
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




activate_record(Id, Attrs) ->
    [Module, _IdNum] = re:split(Id, "-", [{return, list}]),
    Type = list_to_atom(Module),
    DummyRecord = apply(Type, new, lists:seq(1, proplists:get_value(new, Type:module_info(exports)))),
    apply(Type, new, lists:map(fun
                (id) -> Id;
                (Key) -> proplists:get_value(Key, Attrs)
            end, DummyRecord:attribute_names())).
