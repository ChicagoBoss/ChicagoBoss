-module(boss_db_adapter_riak).
-behaviour(boss_db_adapter).
-export([start/0, start/1, stop/1, find/2, find/7]).
-export([count/3, counter/2, incr/2, incr/3, delete/2, save_record/2]).
-export([push/2, pop/2]).

-define(LOG(Name, Value), io:format("DEBUG: ~s: ~p~n", [Name, Value])).

start() ->
    start([]).

start(Options) ->
    % TODO: crypto is needed for unique_id_62/0. Remove it when
    %       unique_id_62/0 is not needed.
    crypto:start(),
    application:start(riakpool),
    Host = proplists:get_value(db_host, Options, "localhost"),
    Port = proplists:get_value(db_port, Options, 8087),
    riakpool:start_pool(Host, Port),
    {ok, undefined}.

stop(_) ->
    riakpool:stop(),
    ok.

find(_, Id) ->
    {Type, Bucket, Key} = infer_type_from_id(Id),
    case riakpool_client:get(Bucket, Key) of
        {ok, Value} ->
            Data = binary_to_term(Value),
            DummyRecord = apply(Type, new, lists:seq(1, proplists:get_value(new,
                                 Type:module_info(exports)))),
            Record = apply(Type, new, lists:map(fun (AttrName) ->
                            proplists:get_value(AttrName, Data)
                    end, DummyRecord:attribute_names())),
            Record:id(Id);
        {error, Reason} ->
            {error, Reason}
    end.

find_acc(_, [], Acc) ->
    lists:reverse(Acc);
find_acc(Prefix, [Id | Rest], Acc) ->
    case find(undefined, Prefix ++ binary_to_list(Id)) of
        {error, _Reason} ->
            find_acc(Prefix, Rest, Acc);

        Value ->
            find_acc(Prefix, Rest, [Value | Acc])
    end.

% this is a stub just to make the tests runable
find(_, Type, Conditions, Max, Skip, Sort, SortOrder) ->
    {ok, Keys} = riakpool_client:list_keys(type_to_bucket_name(Type)),
    AllRecords = find_acc(atom_to_list(Type) ++ "-", Keys, []),
    Records = case Conditions of
        [{Key, 'gt', Value}] -> [Record || Record <- AllRecords,
                                 Record:Key() > Value];
        _ -> AllRecords
    end,
    Sorted = if
        is_atom(Sort) ->
            lists:sort(fun (A, B) ->
                        case SortOrder of
                            num_ascending -> A:Sort() =< B:Sort();
                            str_ascending -> A:Sort() =< B:Sort();
                            num_descending -> A:Sort() > B:Sort();
                            str_descending -> A:Sort() > B:Sort()
                        end
                end,
                Records);
        true -> Records
    end,
    case Max of
        0 -> lists:nthtail(Skip, Sorted);
        _ -> lists:sublist(Sorted, Skip + 1, Max)
    end.

% this is a stub just to make the tests runable
count(Conn, Type, Conditions) ->
    length(find(Conn, Type, Conditions, 0, 0, 0, 0)).

counter(_Conn, _Id) ->
    {error, notimplemented}.

incr(Conn, Id) ->
    incr(Conn, Id, 1).
incr(_Conn, _Id, _Count) ->
    {error, notimplemented}.


delete(_, Id) ->
    {_Type, Bucket, Key} = infer_type_from_id(Id),
    ok = riakpool_client:delete(Bucket, Key).

save_record(_, Record) ->
    Type = element(1, Record),
    Bucket = type_to_bucket_name(Type),
    PropList = [{K, V} || {K, V} <- Record:attributes(), K =/= id],
    Key = case Record:id() of
        id ->
            % TODO: The next release of Riak will support server-side ID
            %       generating. Get rid of unique_id_62/0.
            unique_id_62();
        DefinedId when is_list(DefinedId) ->
            [_ | Tail] = string:tokens(DefinedId, "-"),
            string:join(Tail, "-")
    end,
    ok = riakpool_client:put(list_to_binary(Bucket), list_to_binary(Key),
                             term_to_binary(PropList)),
    {ok, Record:id(atom_to_list(Type) ++ "-" ++ Key)}.

% These 2 functions are not part of the behaviour but are required for
% tests to pass
push(_Conn, _Depth) -> ok.

pop(_Conn, _Depth) -> ok.

% Internal functions

infer_type_from_id(Id) when is_list(Id) ->
    [Type | Tail] = string:tokens(Id, "-"),
    BossId = string:join(Tail, "-"),
    {list_to_atom(Type), type_to_bucket(Type), list_to_binary(BossId)}.

% Find bucket name from Boss type
type_to_bucket(Type) ->
    list_to_binary(type_to_bucket_name(Type)).

type_to_bucket_name(Type) when is_atom(Type) ->
    type_to_bucket_name(atom_to_list(Type));
type_to_bucket_name(Type) when is_list(Type) ->
    inflector:pluralize(Type).

% Unique key generator (copy&pasted from riak_core_util.erl)
% see https://github.com/basho/riak_core/blob/master/src/riak_core_util.erl#L131
% for details.
% TODO: Get rid of this code when server-side ID generating is available
%       in Riak.

%% @spec integer_to_list0(Integer :: integer(), Base :: integer()) ->
%% string()
%% @doc Convert an integer to its string representation in the given
%% base. Bases 2-62 are supported.
integer_to_list0(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list0(I, Base)
  when is_integer(I), is_integer(Base),Base >= 2, Base =< 1+$Z-$A+10+1+$z-$a ->
    if I < 0 ->
            [$-|integer_to_list0(-I, Base, [])];
       true ->
            integer_to_list0(I, Base, [])
    end;
integer_to_list0(I, Base) ->
    erlang:error(badarg, [I, Base]).

%% @spec integer_to_list0(integer(), integer(), string()) -> string()
integer_to_list0(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 36 ->
                [D-36+$a|R0];
            D >= 10 ->
                [D-10+$A|R0];
            true ->
                [D+$0|R0]
         end,
    if I1 =:= 0 ->
            R1;
       true ->
            integer_to_list0(I1, Base, R1)
    end.

%% @spec unique_id_62() -> string()
%% @doc Create a random identifying integer, returning its string
%% representation in base 62.
unique_id_62() ->
    Rand = crypto:sha(term_to_binary({make_ref(), now()})),
    <<I:160/integer>> = Rand,
    integer_to_list0(I, 62).
