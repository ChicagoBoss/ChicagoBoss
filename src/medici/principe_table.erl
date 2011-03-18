%%%-------------------------------------------------------------------
%%% File:      principe_table.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright Copyright (c) 2009, Jim McCoy.  All Rights Reserved.
%%%
%%% @doc
%%% An extension to the principe module that handles tables.  See the
%%% principe module docs for a note about Tyrant and server byte-order
%%% issues.  When using tyrant in table mode this matters far less than
%%% it does for Tyrant in other modes; in most cases Tyrant will encode
%%% table column values internally as strings. The only place that this
%%% matters is using addint or adddouble in conjunction with a row in
%%% which you manually added a magic "_num" column.  For this case you
%%% will need to do a bit of magic on your own to properly encode the
%%% float or int using the put() function.  See the @see principe module
%%% for examples (use the "bigendian" property from a stat() call to
%%% figure out what your server expects.)
%%% @end
%%%-------------------------------------------------------------------

-module(principe_table).

-export([connect/0, connect/1, put/3, putkeep/3, putcat/3, update/3, out/2,
	 get/2, mget/2, vsiz/2, iterinit/1, iternext/1, fwmkeys/3, sync/1, optimize/2,
	 vanish/1, rnum/1, size/1, stat/1, copy/2, restore/3, addint/3, adddouble/3, 
	 adddouble/4, setmst/3, setindex/3, query_limit/3, query_limit/4, query_add_condition/5,
	 query_order/4, search/2, genuid/1, searchcount/2, searchout/2]).

-include("principe.hrl").

%%====================================================================
%% The Tokyo Tyrant access functions
%%====================================================================

%% @spec connect() -> {ok, port()} | error()
%%
%% @doc 
%% Establish a connection to the tyrant service.
%% @end
connect() ->
    connect([]).

%% @spec connect(ConnectProps::proplist()) -> {ok, port()} | error()
%%
%% @doc 
%% Establish a connection to the tyrant service using properties in the
%% ConnectProps proplist to determine the hostname, port number and tcp
%% socket options for the connection.  Any missing parameters are filled
%% in using the module defaults.
%% @end
connect(ConnectProps) ->
    {ok, Socket} = principe:connect(ConnectProps),
    % make sure we are connection to a tyrant server in table mode
    case proplists:get_value(type, principe:stat(Socket)) of
	"table" ->
	    {ok, Socket};
	_ ->
	    {error, no_table_server}
    end.

%%====================================================================
%%  Standard tyrant functions (straight pass-through to principe.erl)
%%====================================================================

%% @spec mget(Socket::port(),
%%            KeyList::keylist()) -> [{Key::binary(), Value::proplist()}] | error()
%%
%% @doc 
%% Get the values for a list of keys.  Due to the way that columns are returned
%% via the tyrant protocol a null seperator is used to break 
mget(Socket, KeyList) ->
    case principe:mget(Socket, KeyList) of
	{error, Reason} ->
	    {error, Reason};
	MgetResults ->
	    lists:keymap(fun(BinaryToSplit) ->
				 columnize_values(binary_to_list(BinaryToSplit), [], []) 
			 end, 2, MgetResults)
    end.


%% @spec columnize_values(ColumnValues::list(),
%%                        Current::list(),
%%                        Stack::[binary()]) -> [{ColumnName::binary(), ColumnValue::binary()}]
%%
%% @private
%% Convert a list of bytes (generally one that was converted running binary_to_list
%% on the results returned from tyrant) into a proplist of column names and values.
%% This function (like tyrant) uses a null value as the separator for column names
%% and values.  A column name that contains a null will cause this function to choke
%% or return invalid data.
%% @end
columnize_values([], Current, Stack) ->
    FinalStack = lists:reverse([list_to_binary(lists:reverse(Current)) | Stack]),
    return_column_vals(FinalStack, []);
columnize_values([0 | T], [], Stack) ->
    columnize_values(T, [], [<<"">>|Stack]);
columnize_values([0 | T], Current, Stack) ->
    columnize_values(T, [], [list_to_binary(lists:reverse(Current)) | Stack]);
columnize_values([H | T], Current, Stack) ->
    columnize_values(T, [H | Current], Stack).

%% @spec return_column_vals(ValuesToParse::list(),
%%                          FinalResult::proplist()) -> proplist()
%%
%% @private Take a list with an even number of elements and make it a proplist
return_column_vals([], Cols) ->
    Cols;
return_column_vals([K, V | Tail], Cols) ->
    return_column_vals(Tail, [{K, V} | Cols]).


%% @spec addint(Socket::port(),
%%              Key::key(),
%%              Int::integer()) -> integer() | error()
%%
%% @doc Add an integer value to the _num column of a given a key.  The
%% _num column will be created if it does not already exist.
%%
%% NOTE: Something truly wierd about this _num column setup is that tc/tyrant
%% expects the column to be a string() value internally.  I am assuming this
%% is because for table databases a null is used as a column separator, if the
%% _num value was stored as an integer then differing server byte order (which TC
%% suffers from) would confuse the server.  If you put() an integer() value to
%% the _num column it will get overwritten by an addint() call, but if you write
%% a integer_to_list(integer()) value to the num column via a normal put() call
%% things will work correctly.
%% @end
addint(Socket, Key, Int) ->
    principe:addint(Socket, Key, Int).

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Double::float()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc Add an float value to the _num column of a given a key.  The
%% _num column will be created if it does not already exist.
%% @end
adddouble(Socket, Key, Double) ->
    principe:adddouble(Socket, Key, Double).

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Integral::integer(),
%%                 Fractional::integer()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc The raw adddouble function for those who need a bit more control on float adds.
adddouble(Socket, Key, Integral, Fractional) ->
    principe:adddouble(Socket, Key, Integral, Fractional).    

%% @spec iterinit(Socket::port()) -> ok | error()
%%
%% @doc Start iteration protocol.  WARNING: The tyrant iteration protocol has no
%% concurrency controls whatsoever, so if multiple clients try to do iteration
%% they will stomp all over each other!
%% @end
iterinit(Socket) ->
    principe:iterinit(Socket).

%% @spec iternext(Socket::port()) -> {Key::binary(), Value::binary()} | error()
%%
%% @doc Get the next key/value pair in the iteration protocol.
iternext(Socket) ->
    principe:iternext(Socket).

%% @spec fwmkeys(Socket::port(),
%%               Prefix::iolist(),
%%               MaxKeys::integer()) -> [binary()]
%%
%% @doc Return a number of keys that match a given prefix.
fwmkeys(Socket, Prefix, MaxKeys) ->
    principe:fwmkeys(Socket, Prefix, MaxKeys).

%% @spec vsiz(Socket::port(),
%%            Key::key()) -> integer()
%%
%% @doc
%% Get the size of the value for a given key.  The value returned for
%% a key will be the total of the column size values, and each column
%% size will be the size of the column name (in bytes), the size of the
%% column value (in bytes), plus one for the internal null seperator 
%% between column name and value plus one for the null terminator for
%% the column (i.e. length(ColumnName) + length(ColumnValue) + 2 for each
%% column.)
%% @end
vsiz(Socket, Key) ->
    principe:vsiz(Socket, Key).

%% @spec sync(Socket::port()) -> ok | error()
%%
%% @doc Call sync() on the remote database.
sync(Socket) ->
    principe:sync(Socket).

%% @spec vanish(Socket::port()) -> ok | error()
%%
%% @doc Remove all records from the remote database.
vanish(Socket) ->
    principe:vanish(Socket).

%% @spec optimize(Socket::port(),
%%                Params::list()) -> ok | error()
%%
%% @doc Change the remote database tuning parameters
optimize(Socket, Params) ->
    principe:optimize(Socket, Params).

%% Get the number of records in the remote database
rnum(Socket) ->
    principe:rnum(Socket).

%% @spec size(Socket::port()) -> integer() | error()
%%
%% @doc Get the size in bytes of the remote database.
size(Socket) ->
    principe:size(Socket).

%% @spec stat(Socket::port()) -> proplist() | error()
%%
%% @doc Get the status string of a remote database.
stat(Socket) ->
    principe:stat(Socket).

%% @spec copy(Socket::port(), 
%%            iolist()) -> ok | error()
%%
%% @doc Make a copy of the database file of the remote database.
copy(Socket, PathName) ->
    principe:copy(Socket, PathName).

%% @spec restore(Socket::port(), 
%%               PathName::iolist(), 
%%               TimeStamp::integer) -> ok | error()
%%
%% @doc Restore the database to a particular point in time from the update log.
restore(Socket, PathName, TimeStamp) ->
    principe:restore(Socket, PathName, TimeStamp).

%% @spec setmst(Socket::port(), 
%%              HostName::iolist(), 
%%              Port::integer) -> ok | error()
%%
%% @doc Set the replication master of a remote database server.
setmst(Socket, HostName, Port) ->
    principe:setmst(Socket, HostName, Port).

%%====================================================================
%%  Table functions
%%====================================================================

%% @spec put(Socket::port(), 
%%           Key::key(), 
%%           Cols::coldata()) -> [] | error()
%%
%% @doc
%% Call the Tyrant server to store a new set of column values for the given key.
%% @end
put(Socket, Key, Cols) ->
    Data = encode_table(Cols),
    ?TSimple(<<"put">>, [Key | Data]).

%% @spec putkeep(Socket::port(), 
%%               Key::key(), 
%%               Cols::coldata()) -> [] | error()
%%
%% @doc 
%% Call the Tyrant server to add a set of column values for a given key.  Will 
%% return an error if Key is already in the remote database.
%% @end
putkeep(Socket, Key, Cols) ->
    Data = encode_table(Cols),
    ?TSimple(<<"putkeep">>, [Key | Data]).

%% @spec putcat(Socket::port(), 
%%              Key::key(), 
%%              Cols::coldata()) -> [] | error()
%%
%% @doc 
%% Concatenate a set of column values to the existing value of Key (or
%% create a new entry for Key with the given column values if Key is not
%% in the remote database.)  If any columns in Cols already have values
%% for the given key then the entries provided in the Cols parameter for
%% those specific columns will be ignored by the remote database. Use the
%% update() function to overwrite existing column values.
%% @end
putcat(Socket, Key, Cols) ->
    Data = encode_table(Cols),
    ?TSimple(<<"putcat">>, [Key | Data]).

%% @spec update(Socket::port(), 
%%              Key::key(), 
%%              Cols::coldata()) -> [] | error()
%%
%% @doc 
%% Update a table entry by merging Cols into existing data for given key. The
%% end result of this function should be to create a new entry for Key whose
%% column values are the new data from the Cols parameter as well as any previous
%% columns for Key that were not in the Cols proplist.
%% @end
%%
%% TODO: better way would be to use a lua server script to perform the merge?
update(Socket, Key, Cols) ->
    case principe:misc(Socket, <<"get">>, [Key]) of
	{error, _Reason} ->
	    UpdatedProps = Cols;
	ExistingData ->
	    OldProps = decode_table(ExistingData),
	    NewProps = lists:foldl(fun({K, V}, AccIn) when is_list(K) ->
					   [{list_to_binary(K), V} | AccIn];
				      ({K, V}, AccIn) when is_atom(K) ->
					   [{list_to_binary(atom_to_list(K)), V} | AccIn];
				      (Other, AccIn) -> [Other | AccIn]
				   end, OldProps, Cols),
	    UpdatedProps = [{K, proplists:get_value(K, NewProps)} || K <- proplists:get_keys(NewProps)]
    end,
    Data = encode_table(UpdatedProps),
    ?TSimple(<<"put">>, [Key | Data]).

%% @spec out(Socket::port(), 
%%           Key::key()) -> ok | error()
%%
%% @doc 
%% Remove a key from the remote database.  Will return an error if Key is
%% not in the database.
%% @end
out(Socket, Key) ->
    ?TSimple(<<"out">>, [Key]).

%% @spec get(Socket::port(), 
%%           Key::key()) -> proplist() | error()
%%
%% @doc Get the value for a given key. Table data is returned in a proplist of
%% {ColumnName, ColumnValue} tuples.
%% @end
get(Socket, Key) ->
    case ?TRaw(<<"get">>, [Key]) of 
	{error, Reason} -> 
	    {error, Reason}; 
	RecList ->
	    decode_table(RecList)
    end.

%% @spec setindex(Socket::port(),
%%                ColName::index_col(),
%%                Type::index_type()) -> ok | error()
%%
%% @doc
%% Tell the tyrant server to build an index for a column.  The ColName
%% should be either the atom "primary" (to index on the primary key) or a 
%% iolist() that names the column to be indexed. Type should be an atom
%% selected from decimal (index column as decimal data), lexical (index as
%% character/string data) or void (remove an existing index for ColName).
%% @end
setindex(Socket, primary, Type) when is_atom(Type) ->
    ?TSimple(<<"setindex">>, [?NULL, setindex_request_val(Type)]);
setindex(Socket, ColName, Type) when is_atom(Type) ->
    ?TSimple(<<"setindex">>, [ColName, setindex_request_val(Type)]).

%% @spec genuid(Socket::port()) -> binary() | error()
%%
%% @doc Generate a unique id within the set of primary keys
genuid(Socket) ->
    case ?TRaw(<<"genuid">>, []) of
	[NewId] ->
	    NewId;
	Error ->
	    Error
    end.

%% @spec query_add_condition(Query::proplist(),
%%                       ColName::iolist(),
%%                       Op::query_opcode(),
%%                       ExprList::query_expr()) -> proplist()
%%
%% @doc
%% Add a condition for a query.  ExprList should be a list of one or more
%% values where each value is either a binary, string, or integer.  Op can be
%% either an atom or a tuple of atoms describing the operation.  If the first
%% atom in an Op tuple is "no" then the condition is a negation query and if
%% the last atom is no_index an existing index on the remote database server will
%% be bypassed.
%% @end
query_add_condition(_Sock, Query, ColName, Op, ExprList) when is_list(ExprList) ->
    [{{add_cond, ColName, Op, ExprList}, 
      ["addcond", 
       ?NULL, 
       ColName, 
       ?NULL, 
       integer_to_list(add_condition_op_val(Op)), 
       ?NULL, 
       convert_query_exprlist(ExprList)]
     } | Query].

%% @spec query_limit(Query::proplist(),
%%                   Max::integer(),
%%                   Skip::integer()) -> proplist()
%%
%% @doc Set a limit on the number of returned values for Query, skip the first Skip records.
query_limit(_Sock, Query, Max, Skip) when is_integer(Max), Max > 0, is_integer(Skip), Skip >= 0 ->
    LimitKey = {set_limit, Max, Skip},
    LimitValue = ["setlimit", 
		?NULL, 
		integer_to_list(Max), 
		?NULL, 
		integer_to_list(Skip)],
    case lists:keysearch(set_limit, 1, proplists:get_keys(Query)) of
	false ->
	    [{LimitKey, LimitValue} | Query];
	{value, ExistingKey} ->
	    [{LimitKey, LimitValue} | proplists:delete(ExistingKey, Query)]
    end.

%% @spec query_limit(Query::proplist(),
%%                   Max::integer()) -> proplist()
%%
%% @doc Set a limit on the number of returned values for Query.
%%
%% XXX: should the missing skip be 0 or -1 (protocol ref and perl versions seem to disagree)
query_limit(_Sock, Query, Max) ->
    query_limit(_Sock, Query, Max, 0).

%% @spec query_order(Query::proplist(),
%%                   ColName::index_col(),
%%                   Type::order_type()) -> proplist()
%%
%% @doc Set the order for returned values in Query.
query_order(_Sock, Query, primary, Type) when is_atom(Type) ->
    OrderKey = {set_order, primary, Type},
    OrderValue = ["setorder", 
		  ?NULL, 
		  "",
		  ?NULL, 
		  integer_to_list(order_request_val(Type))],
    case lists:keysearch(set_order, 1, proplists:get_keys(Query)) of
	false ->
	    [{OrderKey, OrderValue} | Query];
	{value, ExistingKey} ->
	    [{OrderKey, OrderValue} | proplists:delete(ExistingKey, Query)]
    end;
query_order(_Sock, Query, ColName, Type) when is_atom(Type) ->
    OrderKey = {set_order, ColName, Type},
    OrderValue = ["setorder", 
		  ?NULL, 
		  ColName, 
		  ?NULL, 
		  integer_to_list(order_request_val(Type))],
    case lists:keysearch(set_order, 1, proplists:get_keys(Query)) of
	false ->
	    [{OrderKey, OrderValue} | Query];
	{value, ExistingKey} ->
	    [{OrderKey, OrderValue} | proplists:delete(ExistingKey, Query)]
    end.

%% @spec search(Socket::port,
%%              Query::proplist()) -> keylist() | error()
%%
%% @doc Run a prepared query against the table and return matching keys.
search(Socket, Query) ->
    ?TRaw(<<"search">>, [V || {_K, V}=Prop <- Query, is_tuple(Prop), erlang:size(Prop)==2]).

%% @spec searchcount(Socket::port,
%%                   Query::proplist()) -> [integer()] | error()
%%
%% @doc Run a prepared query against the table and get the count of matching keys.
searchcount(Socket, Query) ->
    case ?TRaw(<<"search">>, [V || {_K, V}=Prop <- Query, is_tuple(Prop), erlang:size(Prop)==2] ++ ["count"]) of
	{error, Reason} ->
	    {error, Reason};
	[] ->
	    0;
	[Count] ->
	    list_to_integer(binary_to_list(Count))
    end.

%% @spec searchout(Socket::port,
%%                 Query::proplist()) -> ok | error()
%%
%% @doc Run a prepared query against the table and remove the matching records.
searchout(Socket, Query) ->
    ?TSimple(<<"search">>, [V || {_K, V}=Prop <- Query, is_tuple(Prop), erlang:size(Prop)==2] ++ ["out"]).

%% %% Run a prepared query against the table and get the matching records.  Due
%% %% to protocol restraints, the returned result cannot include columns whose
%% %% name or value include the null (0x0) character.
%% tblsearchget(Socket, TblQuery) ->
%%     void.

%% tblrescols(Socket, TblQuery) ->
%%     void.

 
%%====================================================================
%%  Table utility functions
%%====================================================================

%% @spec add_condition_op_val(query_op()) -> integer()
%%
%% @private Decode add_contition operation tag
add_condition_op_val({no, Op}) when is_atom(Op) ->
    ?QCNEGATE bor add_condition_op_val(Op);
add_condition_op_val({Op, no_index}) when is_atom(Op) ->
    ?QCNOIDX bor add_condition_op_val(Op);
add_condition_op_val({no, Op, no_index}) when is_atom(Op)->
    ?QCNEGATE bor ?QCNOIDX bor add_condition_op_val(Op);
add_condition_op_val({Op}) when is_atom(Op) ->
    add_condition_op_val(Op);
add_condition_op_val(Op) when is_atom(Op) ->
    case Op of
	str_eq ->
	    ?QCSTREQ;
	str_inc ->
	    ?QCSTRINC;
	str_begin ->
	    ?QCSTRBW;
	str_end ->
	    ?QCSTREW;
	str_and ->
	    ?QCSTRAND;
	str_or ->
	    ?QCSTROR;
	str_in_list ->
	    ?QCSTROREQ;
	str_regex ->
	    ?QCSTRRX;
	num_eq ->
	    ?QCNUMEQ;
	num_gt ->
	    ?QCNUMGT;
	num_ge ->
	    ?QCNUMGE;
	num_lt ->
	    ?QCNUMLT;
	num_le ->
	    ?QCNUMLE;
	num_between ->
	    ?QCNUMBT;
	num_in_list ->
	    ?QCNUMOREQ
    end.

%% @spec setindex_request_val(index_type()) -> integer()
%%
%% @private Decode set_index request tag
setindex_request_val(Type) ->
    case Type of
	lexical ->
	    ?ITLEXICAL;
	decimal ->
	    ?ITDECIMAL;
	optimized ->
	    ?ITOPT;
	void ->
	    ?ITVOID
    end.

%% @spec order_request_val(order_type()) -> integer()
%%
%% @private Decode result order tag
order_request_val(Type) ->
    case Type of
	str_ascending ->
	    ?QOSTRASC;
	str_descending ->
	    ?QOSTRDESC;
	num_ascending ->
	    ?QONUMASC;
	num_descending ->
	    ?QONUMDESC
    end.

%% @spec convert_query_exprlist(query_expr()) -> [string()]
%%
%% @private 
%% Convert query expression list to comma-seperated list of string values.
convert_query_exprlist(ExprList) ->
    convert_query_exprlist(ExprList, []).

convert_query_exprlist([H | T], []) when is_integer(H) ->
    convert_query_exprlist(T, [integer_to_list(H)]);
convert_query_exprlist([H | T], []) when is_binary(H) ->
    convert_query_exprlist(T, [binary_to_list(H)]);
convert_query_exprlist([H | T], []) ->
    convert_query_exprlist(T, [H]);
convert_query_exprlist([H | T], Acc) when is_integer(H) ->
    convert_query_exprlist(T, [integer_to_list(H) | ["," | Acc]]);
convert_query_exprlist([H | T], Acc) when is_binary(H) ->
    convert_query_exprlist(T, [binary_to_list(H) | ["," | Acc]]);
convert_query_exprlist([H | T], Acc) ->
    convert_query_exprlist(T, [H | ["," | Acc]]);
convert_query_exprlist([], Acc) ->
    lists:reverse(Acc).

%% @spec encode_table(proplist()) -> [value_or_num()]
%%
%% @private Convert proplist to a list of key, value sequences.
encode_table(Data) when is_list(Data) ->
    encode_table(Data, []).

encode_table([], Acc) ->
    lists:reverse(Acc);
encode_table([{K, V} | Tail], Acc) ->
    encode_table(Tail, [V | [ K | Acc]]).

%% @spec decode_table([value_or_num()]) -> proplist() | error()
%%
%% @private Convert list of key, value pairs to a proplist
decode_table({error, Code}) ->
    {error, Code};
decode_table(Data) when is_list(Data) ->
    decode_table(Data, []).

decode_table([], Acc) ->
    lists:reverse(Acc);
decode_table([K, V | Tail], Acc) ->
    decode_table(Tail, [{K, V} | Acc]).

%% Some standard types for edoc
%%
%% @type endian() = big | little
%% @type key() = iolist()
%% @type value() = iolist()
%% @type value_or_num() = iolist() | integer() | float()
%% @type keylist() = [key()]
%% @type coldata() = [{key(), value_or_num()}]
%% @type error() = {error, term()}
%% @type index_col() = primary | iolist()
%% @type index_type() = lexical | decimal | void
%% @type query_opcode() = atom() | tuple()
%% @type query_expr() = [binary() | string() | integer()]
%% @type order_type() = str_ascending | str_descending | num_ascending | num_descending

%% EUnit tests
%%
-ifdef(EUNIT).
test_setup() ->
    {ok, Socket} = ?MODULE:connect(),
    ok = ?MODULE:vanish(Socket),
    Socket.

test_setup_with_data() ->
    Socket = test_setup(),
    ColData = [{"rec1", [{"name", "alice"}, {"sport", "baseball"}]},
	       {"rec2", [{"name", "bob"}, {"sport", "basketball"}]},
	       {"rec3", [{"name", "carol"}, {"age", "24"}]},
	       {"rec4", [{"name", "trent"}, {"age", "33"}, {"sport", "football"}]},
	       {"rec5", [{"name", "mallet"}, {"sport", "tennis"}, {"fruit", "apple"}]}
	       ],
    lists:foreach(fun({Key, ValProplist}) ->
			  ok = ?MODULE:put(Socket, Key, ValProplist)
		  end, ColData),
    Socket.

put_get_test() ->
    Socket = test_setup_with_data(),
    ?assertMatch([{<<"age">>, <<"24">>}, {<<"name">>, <<"carol">>}], lists:sort(?MODULE:get(Socket, "rec3"))),
    ok = ?MODULE:put(Socket, <<"put_get1">>, [{"num", 32}]),
    % Note that by default integers go over to Tyrant in network byte-order
    ?assertMatch([{<<"num">>, <<32:32>>}], lists:sort(?MODULE:get(Socket, <<"put_get1">>))),
    ok.

putkeep_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, "putkeep1", [{"col1", "testval1"}]),
    ?assertMatch([{<<"col1">>, <<"testval1">>}], ?MODULE:get(Socket, "putkeep1")),
    ?assertMatch({error, _}, ?MODULE:putkeep(Socket, <<"putkeep1">>, [{"col1", "testval2"}])),
    ?assertMatch([{<<"col1">>, <<"testval1">>}], ?MODULE:get(Socket, "putkeep1")),
    ok = ?MODULE:putkeep(Socket, <<"putkeep2">>, [{"col1", "testval2"}]),
    ?assertMatch([{<<"col1">>, <<"testval2">>}], ?MODULE:get(Socket, "putkeep2")),
    ok.

putcat_test() ->
    Socket = test_setup_with_data(),
    ?assertMatch([{<<"age">>, <<"24">>}, {<<"name">>, <<"carol">>}], 
		 lists:sort(?MODULE:get(Socket, "rec3"))),
    ok = ?MODULE:putcat(Socket, "rec3", [{"sport", "golf"}]),
    ?assertMatch([{<<"age">>, <<"24">>}, {<<"name">>, <<"carol">>}, {<<"sport">>, <<"golf">>}], 
		 lists:sort(?MODULE:get(Socket, "rec3"))),
    ok.

update_test() ->
    Socket = test_setup_with_data(),
    ?assertMatch([{<<"name">>, <<"alice">>}, {<<"sport">>, <<"baseball">>}], ?MODULE:get(Socket, "rec1")),
    ok = ?MODULE:update(Socket, "rec1", [{"sport", "swimming"}, {"pet", "dog"}]),
    ?assertMatch([{<<"name">>, <<"alice">>}, {<<"pet">>, <<"dog">>}, {<<"sport">>, <<"swimming">>}],
		 lists:sort(?MODULE:get(Socket, "rec1"))),
    ok.

out_test() ->
    Socket = test_setup_with_data(),
    ok = ?MODULE:out(Socket, <<"rec1">>),
    ?assertMatch({error, _}, ?MODULE:get(Socket, <<"rec1">>)),
    ok.

vsiz_test() ->
    Socket = test_setup(),
    ColName = "col1",
    ColVal = "vsiz test",
    ok = ?MODULE:put(Socket, "vsiz1", [{ColName, ColVal}]),
    %% size = col + null sep + val + null column stop
    ExpectedLength = length(ColName) + length(ColVal) + 2,
    ?assert(?MODULE:vsiz(Socket, "vsiz1") =:= ExpectedLength),
    ColName2 = "another col",
    ColVal2 = "more bytes",
    ok = ?MODULE:put(Socket, "vsiz2", [{ColName, ColVal}, {ColName2, ColVal2}]),
    ExpectedLength2 = ExpectedLength + length(ColName2) + length(ColVal2) + 2,
    ?assert(?MODULE:vsiz(Socket, "vsiz2") =:= ExpectedLength2),
    ok.

vanish_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, "vanish1", [{"col1", "going away"}]),
    ok = ?MODULE:vanish(Socket),
    ?assertMatch({error, _}, ?MODULE:get(Socket, "vanish1")),
    ok.

addint_test() ->
    Socket = test_setup(),
    ?assert(?MODULE:addint(Socket, "addint1", 100) =:= 100),
    ok = ?MODULE:put(Socket, "addint2", [{"_num", "10"}]), % see ?MODULE:addint edoc for why a string() is used
    ?assert(?MODULE:addint(Socket, "addint2", 10) =:= 20),
    ?assertMatch([{<<"_num">>, <<"100">>}], ?MODULE:get(Socket, "addint1")),
    ?assertMatch([{<<"_num">>, <<"20">>}], ?MODULE:get(Socket, "addint2")),
    ok.

sync_test() ->
    Socket = test_setup(),
    ok = ?MODULE:sync(Socket),
    ok.

rnum_test() ->
    Socket = test_setup_with_data(),
    ?assert(?MODULE:rnum(Socket) =:= 5),
    ok = ?MODULE:out(Socket, "rec1"),
    ?assert(?MODULE:rnum(Socket) =:= 4),
    ok = ?MODULE:vanish(Socket),
    ?assert(?MODULE:rnum(Socket) =:= 0),
    ok.

size_test() ->
    Socket = test_setup(),
    ?MODULE:size(Socket),
    ok.

stat_test() ->
    Socket = test_setup(),
    ?MODULE:stat(Socket),
    ok.

mget_test() ->
    Socket = test_setup_with_data(),
    MGetData = ?MODULE:mget(Socket, ["rec1", "rec3", "rec5"]),
    ?assertMatch([{<<"name">>, <<"alice">>},{<<"sport">>, <<"baseball">>}], 
		 lists:sort(proplists:get_value(<<"rec1">>, MGetData))),
    ?assert(proplists:get_value(<<"rec2">>, MGetData) =:= undefined),
    ?assertMatch([<<"rec1">>, <<"rec3">>, <<"rec5">>], lists:sort(proplists:get_keys(MGetData))),
    ok.

iter_test() ->
    Socket = test_setup_with_data(),
    AllKeys = [<<"rec1">>, <<"rec2">>, <<"rec3">>, <<"rec4">>, <<"rec5">>],
    ok = ?MODULE:iterinit(Socket),
    First = ?MODULE:iternext(Socket),
    ?assert(lists:member(First, AllKeys)),
    IterAll = lists:foldl(fun(_Count, Acc) -> [?MODULE:iternext(Socket) | Acc] end, 
			  [First], 
			  lists:seq(1, length(AllKeys)-1)),
    ?assertMatch(AllKeys, lists:sort(IterAll)),
    ?assertMatch({error, _}, ?MODULE:iternext(Socket)),
    ok.

fwmkeys_test() ->
    Socket = test_setup_with_data(),
    ok = ?MODULE:put(Socket, "fwmkeys1", [{"foo", "bar"}]),
    ?assert(length(?MODULE:fwmkeys(Socket, "rec", 4)) =:= 4),
    ?assert(length(?MODULE:fwmkeys(Socket, "rec", 8)) =:= 5),
    ?assertMatch([<<"fwmkeys1">>], ?MODULE:fwmkeys(Socket, "fwm", 3)),
    ?assertMatch([<<"rec1">>, <<"rec2">>, <<"rec3">>], ?MODULE:fwmkeys(Socket, "rec", 3)),
    ok.

query_generation_test() ->
    ?assertMatch([{{set_order, primary, str_descending}, ["setorder", <<0:8>>, "", <<0:8>>, "1"]}],
		 ?MODULE:query_order([], primary, str_descending)),
    ?assertMatch([{{set_order, "foo", str_ascending}, ["setorder", <<0:8>>, "foo", <<0:8>>, "0"]}],
		 ?MODULE:query_order([{{set_order, blah}, ["foo"]}], "foo", str_ascending)),
    ?assertMatch([{{set_limit, 2, 0}, ["setlimit", <<0:8>>, "2", <<0:8>>, "0"]}],
		 ?MODULE:query_limit([], 2)),
    ?assertMatch([{{set_limit, 4, 1}, ["setlimit", <<0:8>>, "4", <<0:8>>, "1"]}],
		?MODULE:query_limit([{{set_limit, blah}, ["foo"]}], 4, 1)),
    ?assertMatch([{{add_cond, "foo", str_eq, ["bar"]}, ["addcond", <<0:8>>, "foo", <<0:8>>, "0", <<0:8>>, ["bar"]]}],
		 ?MODULE:query_condition([], "foo", str_eq, ["bar"])),
    ?assertMatch([{{add_cond, "foo", {no, str_and}, ["bar","baz"]}, 
		   ["addcond", <<0:8>>, "foo", <<0:8>>, "16777220", <<0:8>>, ["bar",",","baz"]]}],
		 ?MODULE:query_condition([], "foo", {no, str_and}, ["bar", "baz"])),
    ok.

search_test() ->
    Socket = test_setup_with_data(),
    Query1 = ?MODULE:query_condition([], "name", str_eq, ["alice"]),
    ?assertMatch([<<"rec1">>], ?MODULE:search(Socket, Query1)),
    Query2 = ?MODULE:query_condition([], "name", {no, str_eq}, ["alice"]),
    Query2A = ?MODULE:query_limit(Query2, 2),
    ?assertMatch([<<"rec2">>, <<"rec3">>], ?MODULE:search(Socket, Query2A)),
    Query3 = ?MODULE:query_condition([], "age", num_ge, [25]),
    ?assertMatch([<<"rec4">>], ?MODULE:search(Socket, Query3)),
    Query4 = ?MODULE:query_condition([], "name", {no, str_eq}, ["alice"]),
    Query4A = ?MODULE:query_order(Query4, "name", str_descending),
    ?assertMatch([<<"rec4">>, <<"rec5">>, <<"rec3">>, <<"rec2">>], ?MODULE:search(Socket, Query4A)),
    Query5 = ?MODULE:query_order([], primary, str_descending),
    ?assertMatch([<<"rec5">>, <<"rec4">>, <<"rec3">>, <<"rec2">>, <<"rec1">>], ?MODULE:search(Socket, Query5)),
    ok.

searchcount_test() ->
    Socket = test_setup_with_data(),
    Query1 = ?MODULE:query_condition([], "name", str_or, ["alice", "bob"]),
    ?assert(?MODULE:searchcount(Socket, Query1) =:= 2), 
    ok.

searchout_test() ->
    Socket = test_setup_with_data(),
    ?assert(?MODULE:rnum(Socket) =:= 5),
    %% Also testing regex matches, should hit "baseball" and "basketball" but
    %% skip "football"
    Query1 = ?MODULE:query_condition([], "sport", str_regex, ["^ba"]),
    ok = ?MODULE:searchout(Socket, Query1),
    ?assert(?MODULE:rnum(Socket) =:= 3),
    ?assertMatch({error, _}, ?MODULE:get(Socket, "rec1")),
    ok.
-endif.
