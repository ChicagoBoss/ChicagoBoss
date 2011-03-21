-module(boss_db_mock_controller).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_db_mock}, ?MODULE, Args, []).

init(_Options) ->
    {ok, [{dict:new(), 1}]}.

handle_call({find, Id}, _From, [{Dict, _IdCounter}|_] = State) ->
    Reply = case dict:find(Id, Dict) of
        {ok, Record} -> Record;
        error -> undefined
    end,
    {reply, Reply, State};
handle_call({find, Type, Conditions, Max, Skip, SortBy, SortOrder}, _From, [{Dict, _IdCounter}|_] = State) ->
    Records = do_find(Dict, Type, Conditions, Max, Skip, SortBy, SortOrder),
    {reply, Records, State};
handle_call({count, Type, Conditions}, _From, [{Dict, _IdCounter}|_] = State) ->
    Records = do_find(Dict, Type, Conditions, 1000 * 1000 * 1000 * 1000, 0, id, str_ascending),
    {reply, length(Records), State};
handle_call({delete, Id}, _From, [{Dict, IdCounter}|OldState]) ->
    {reply, ok, [{dict:erase(Id, Dict), IdCounter}|OldState]};
handle_call({counter, Id}, _From, [{Dict, _IdCounter}|_] = State) ->
    Value = case dict:find(Id, Dict) of
        {ok, Integer} when is_integer(Integer) ->
            Integer;
        _ ->
            0
    end,
    {reply, Value, State};
handle_call({incr, Id, Amount}, _From, [{Dict, IdCounter}|OldState]) ->
    NewValue = case dict:find(Id, Dict) of
        {ok, OldValue} when is_integer(OldValue) ->
            OldValue + Amount;
        _ ->
            Amount
    end,
    {reply, NewValue, [{dict:store(Id, NewValue, Dict), IdCounter}|OldState]};
handle_call({save_record, Record}, _From, [{Dict, IdCounter}|OldState]) ->
    Type = element(1, Record),
    {Id, IdCounter1} = case Record:id() of
        id -> {lists:concat([Type, "-", IdCounter]), IdCounter + 1};
        Other -> {Other, IdCounter}
    end,
    NewAttributes = lists:map(fun
            ({id, _}) ->
                {id, Id};
            ({Attr, {_, _, _} = Val}) ->
                {Attr, calendar:now_to_datetime(Val)};
            (Other) ->
                Other
        end, Record:attributes()),
    NewRecord = Record:attributes(NewAttributes),
    {reply, NewRecord, [{dict:store(Id, NewRecord, Dict), IdCounter1}|OldState]};
handle_call(push, _From, [{Dict, IdCounter}|_] = State) ->
    {reply, ok, [{Dict, IdCounter}|State]};
handle_call(pop, _From, [_|OldState]) ->
    {reply, ok, OldState};
handle_call(dump, _From, [{Dict, _IdCounter}|_]=State) ->
    {reply, dict:to_list(Dict), State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

do_find(Dict, Type, Conditions, Max, Skip, SortBy, SortOrder) ->
    lists:sublist(lists:nthtail(Skip, 
            lists:sort(fun(RecordA, RecordB) ->
                        AttributeA = sortable_attribute(RecordA, SortBy),
                        AttributeB = sortable_attribute(RecordB, SortBy),
                        case SortOrder of
                            str_ascending ->
                                AttributeA < AttributeB;
                            str_descending ->
                                AttributeA > AttributeB;
                            num_ascending ->
                                AttributeA < AttributeB;
                            num_descending ->
                                AttributeA > AttributeB
                        end
                end,
                lists:map(fun({_, V}) -> V end,
                    dict:to_list(dict:filter(
                            fun(_Id, Record) when is_tuple(Record) ->
                                    element(1, Record) =:= Type andalso
                                    match_cond(Record, Conditions);
                                (_Id, _) ->
                                    false
                            end, Dict))))), Max).

match_cond(_Record, []) ->
    true;
match_cond(Record, [{Key, 'equals', Value}|Rest]) ->
    Record:Key() =:= Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_equals', Value}|Rest]) ->
    Record:Key() =/= Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'in', Value}|Rest]) when is_list(Value) ->
    lists:member(Record:Key(), Value) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_in', Value}|Rest]) when is_list(Value) ->
    (not lists:member(Record:Key(), Value)) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'in', {Min, Max}}|Rest]) when Max >= Min ->
    Record:Key() >= Min andalso Record:Key() =< Max andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_in', {Min, Max}}|Rest]) when Max >= Min ->
    (not (Record:Key() >= Min andalso Record:Key() =< Max)) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'gt', Value}|Rest]) ->
    Record:Key() > Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'lt', Value}|Rest]) ->
    Record:Key() < Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'ge', Value}|Rest]) ->
    Record:Key() >= Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'le', Value}|Rest]) ->
    Record:Key() =< Value andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'matches', "*"++Value}|Rest]) ->
    re:run(Record:Key(), Value, [caseless]) =/= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'matches', Value}|Rest]) ->
    re:run(Record:Key(), Value, []) =/= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_matches', "*"++Value}|Rest]) ->
    re:run(Record:Key(), Value, [caseless]) =:= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_matches', Value}|Rest]) ->
    re:run(Record:Key(), Value, []) =:= nomatch andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains', Value}|Rest]) ->
    lists:member(Value, string:tokens(Record:Key(), " ")) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_contains', Value}|Rest]) ->
    (not lists:member(Value, string:tokens(Record:Key(), " "))) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains_all', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    lists:all(fun(Token) -> lists:member(Token, Tokens) end, Value) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'not_contains_all', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    (not lists:all(fun(Token) -> lists:member(Token, Tokens) end, Value)) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains_any', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    lists:any(fun(Token) -> lists:member(Token, Tokens) end, Value) andalso match_cond(Record, Rest);
match_cond(Record, [{Key, 'contains_none', Value}|Rest]) ->
    Tokens = string:tokens(Record:Key(), " "),
    (not lists:any(fun(Token) -> lists:member(Token, Tokens) end, Value)) andalso match_cond(Record, Rest).

sortable_attribute(Record, Attr) ->
    case Record:Attr() of
        {D, T} when is_tuple(D), is_tuple(T) ->
            calendar:datetime_to_gregorian_seconds({D, T});
        Now when is_tuple(Now) ->
            calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now));
        Bin when is_binary(Bin) ->
            binary_to_list(Bin);
        true -> 1;
        false -> 0;
        Other ->
            Other
    end.
