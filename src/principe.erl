%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% File:      principe.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright Copyright (c) 2009, Jim McCoy.  All Rights Reserved.
%%%
%%% @doc
%%% A thin Erlang wrapper for the Tokyo Tyrant network database protocol.
%%% Requires a Tyrant server that uses the 0.91 protocol version (Tyrant
%%% servers of version 1.1.23 and beyond.)  
%%%
%%% Note: The Tyrant protocol is sensitive to endianness. Specifically, while
%%% the server will take in data in network-order it will store it internally
%%% in big or little endianness depending on the architecture that the Tyrant 
%%% server is running on.  For many use-cases this is not a problem, but if you
%%% plan on storing an int or float and then operating on it using the addint,
%%% adddouble, or script extension functions which may just grab the internal
%%% data directly then it is necessary to store numbers in the proper format for
%%% the remote database. In these cases use the put* and adddouble functions which
%%% take an additional parameter to specify endianness of the remote database.
%%% There are also versions of the misc() and misc_no_update() functions that
%%% specify remote database byte-order for proper encoding of integers and floats
%%% in those functions.
%%% @end
%%%-------------------------------------------------------------------

-module(principe).
-export([connect/0, 
	 connect/1, 
	 put/3, 
	 put/4,
	 putkeep/3,
	 putkeep/4,
	 putcat/3,
	 putcat/4,
	 putshl/4,
	 putshl/5,
	 putnr/3,
	 putnr/4,
	 out/2, 
	 get/2, 
	 mget/2, 
	 vsiz/2, 
	 iterinit/1, 
	 iternext/1, 
	 fwmkeys/3,
	 addint/3,
	 adddouble/3, 
	 adddouble/4,
	 adddouble/5,
	 sync/1,
	 optimize/2,
	 vanish/1, 
	 rnum/1, 
	 size/1, 
	 stat/1,
	 copy/2, 
	 restore/3,
	 restore_with_check/3,
	 setmst/3,
	 misc/3,
	 misc/4,
	 misc_no_update/3,
	 misc_no_update/4,
	 ext/5]).

%% Standard definitions
-define(TSERVER, "localhost").
-define(TPORT, 1978).
-define(TOPTS, [binary, {packet, 0}, {nodelay, true}, {active, true}, {keepalive, true}]).
-define(TIMEOUT, 5000).

%% Tyrant protocol constants
-define(PUT, 16#C810).
-define(PUTKEEP, 16#C811).
-define(PUTCAT, 16#C812).
-define(PUTSHL, 16#C813).
-define(PUTNR, 16#C818).
-define(OUT, 16#C820).
-define(GET, 16#C830).
-define(MGET, 16#C831).
-define(VSIZ, 16#C838).
-define(ITERINIT, 16#C850).
-define(ITERNEXT, 16#C851).
-define(FWMKEYS, 16#C858).
-define(ADDINT, 16#C860).
-define(ADDDOUBLE, 16#C861).
-define(EXT, 16#C868).
-define(SYNC, 16#C870).
-define(OPTIMIZE, 16#C871).
-define(VANISH, 16#C872).
-define(COPY, 16#C873).
-define(RESTORE, 16#C874).
-define(SETMST, 16#C878).
-define(RNUM, 16#C880).
-define(SIZE, 16#C881).
-define(STAT, 16#C888).
-define(MISC, 16#C890).
-define(REPL, 16#C8A0).

-define(MONOULOG, 1 bsl 0).
-define(XOLCKREC, 1 bsl 0).
-define(XOLCKGLB, 1 bsl 1).

%% Macros for function patterns that are used frequently.
-define(T0(Code), gen_tcp:send(Socket, [<<Code:16>>])).
-define(T1(Code), gen_tcp:send(Socket, [<<Code:16>>, <<(iolist_size(Key)):32>>, Key])).
-define(T2(Code), gen_tcp:send(Socket, [<<Code:16>>, <<(iolist_size(Key)):32>>, <<(iolist_size(Value)):32>>, Key, Value])).
-define(R_SUCCESS, tyrant_response(Socket, fun recv_success/2)).
-define(R_INT32, tyrant_response(Socket, fun recv_size/2)).
-define(R_SIZE_DATA, tyrant_response(Socket, fun recv_size_data/2)).
-define(R_INT64, tyrant_response(Socket, fun recv_size64/2)).

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
    Hostname = proplists:get_value(hostname, ConnectProps, ?TSERVER),
    Port = proplists:get_value(port, ConnectProps, ?TPORT),
    Opts = proplists:get_value(connect_opts, ConnectProps),
    case Opts of
	undefined ->
	    gen_tcp:connect(Hostname, Port, ?TOPTS);
	_ ->
	    gen_tcp:connect(Hostname, Port, Opts)
    end.

%% @spec put(Socket::port(), 
%%           Key::key(), 
%%           Value::value_or_num()) -> ok | error()
%%
%% @doc
%% Call the Tyrant server to store a new value for the given key.
%% @end
put(Socket, Key, Value) when is_integer(Value), Value < 4294967296 ->
    put(Socket, Key, <<Value:32>>);
put(Socket, Key, Value) when is_float(Value) ->
    put(Socket, Key, <<Value:64/float>>);
put(Socket, Key, Value) ->
    ?T2(?PUT),							     
    ?R_SUCCESS.

%% @spec put(Socket::port(), 
%%           Key::key(), 
%%           Value::value_or_num(),
%%           endian()) -> ok | error()
%%
%% @doc
%% Call the Tyrant server to store a new value for the given key, the
%% fourth parameter determines how integer and float values are stored
%% on the remote database.
%% @end
put(Socket, Key, Value, big) ->
    put(Socket, Key, Value);
put(Socket, Key, Value, little) when is_integer(Value) ->
    put(Socket, Key, <<Value:32/little>>);
put(Socket, Key, Value, little) when is_float(Value) ->
    put(Socket, Key, <<Value:64/little-float>>);
put(Socket, Key, Value, little) ->
    put(Socket, Key, Value).

%% @spec putkeep(Socket::port(), 
%%               Key::key(), 
%%               Value::value_or_num()) -> ok | error()
%%
%% @doc 
%% Call the Tyrant server to put a new key/value pair into the remote 
%% database.  Will return an error if there is already a value for the
%% Key provided.
%% @end
putkeep(Socket, Key, Value) when is_integer(Value), Value < 4294967296 ->
    putkeep(Socket, Key, <<Value:32>>);
putkeep(Socket, Key, Value) when is_float(Value) ->
    putkeep(Socket, Key, <<Value:64/float>>);
putkeep(Socket, Key, Value) ->
    ?T2(?PUTKEEP),
    ?R_SUCCESS.

%% @spec putkeep(Socket::port(), 
%%               Key::key(), 
%%               Value::value_or_num()
%%               endian()) -> ok | error()
%%
%% @doc 
%% Call the Tyrant server to put a new key/value pair into the remote 
%% database.  Will return an error if there is already a value for the
%% Key provided.  The fourth parameter determines endianness for encoding
%% numbers on the remote database.
%% @end
putkeep(Socket, Key, Value, big) ->
    putkeep(Socket, Key, Value);
putkeep(Socket, Key, Value, little) when is_integer(Value), Value < 4294967296 ->
    putkeep(Socket, Key, <<Value:32/little>>);
putkeep(Socket, Key, Value, little) when is_float(Value) ->
    putkeep(Socket, Key, <<Value:64/little-float>>);
putkeep(Socket, Key, Value, little) ->
    putkeep(Socket, Key, Value).

%% @spec putcat(Socket::port(), 
%%              Key::key(), 
%%              Value::value_or_num()) -> ok | error()
%%
%% @doc 
%% Concatenate a value to the end of the current value for a given key
%% that is stored in the remote database.  If Key does not already
%% exist in the database then this call will operate the same as put().
%% @end
putcat(Socket, Key, Value) when is_integer(Value), Value < 4294967296 ->
    putcat(Socket, Key, <<Value:32>>);
putcat(Socket, Key, Value) when is_float(Value) ->
    putcat(Socket, Key, <<Value:64/float>>);
putcat(Socket, Key, Value) ->
    ?T2(?PUTCAT),
    ?R_SUCCESS.

%% @spec putcat(Socket::port(), 
%%              Key::key(), 
%%              Value::value_or_num(),
%%              endian()) -> ok | error()
%%
%% @doc 
%% Concatenate a value to the end of the current value for a given key
%% that is stored in the remote database.  If Key does not already
%% exist in the database then this call will operate the same as put(). 
%% The last parameter determines endian encoding on the remote database.
%% @end
putcat(Socket, Key, Value, big) ->
    putcat(Socket, Key, Value);
putcat(Socket, Key, Value, little) when is_integer(Value), Value < 4294967296 ->
    putcat(Socket, Key, <<Value:32/little>>);
putcat(Socket, Key, Value, little) when is_float(Value) ->
    putcat(Socket, Key, <<Value:64/little-float>>);
putcat(Socket, Key, Value, little) ->
    putcat(Socket, Key, Value).

%% @spec putshl(Socket::port(), 
%%              Key::key(), 
%%              Value::value_or_num(), 
%%              Width::integer()) -> ok | error()
%%
%% @doc 
%% Concatenate a value to a given key in the remote database and shift the
%% resulting value to the left until it is Width bytes long.
%% @end
putshl(Socket, Key, Value, Width) when is_integer(Value), Value < 4294967296 ->
    putshl(Socket, Key, <<Value:32>>, Width);
putshl(Socket, Key, Value, Width) when is_float(Value) ->
    putshl(Socket, Key, <<Value:64/float>>, Width);
putshl(Socket, Key, Value, Width) when is_integer(Width) ->
    gen_tcp:send(Socket, [<<?PUTSHL:16>>, 
			  <<(iolist_size(Key)):32>>, 
			  <<(iolist_size(Value)):32>>, 
			  <<Width:32>>, Key, Value]),
    ?R_SUCCESS.

%% @spec putshl(Socket::port(), 
%%              Key::key(), 
%%              Value::value_or_num(), 
%%              Width::integer(),
%%              endian()) -> ok | error()
%%
%% @doc 
%% Concatenate a value to a given key in the remote database and shift the
%% resulting value to the left until it is Width bytes long.  The last
%% parameter determines byte-order encoding for the remote database.
%% @end
putshl(Socket, Key, Value, Width, big) ->
    putshl(Socket, Key, Value, Width);
putshl(Socket, Key, Value, Width, little) when is_integer(Value), Value < 4294967296 ->
    putshl(Socket, Key, <<Value:32/little>>, Width);
putshl(Socket, Key, Value, Width, little) when is_float(Value) ->
    putshl(Socket, Key, <<Value:64/little-float>>, Width);
putshl(Socket, Key, Value, Width, little) ->
    putshl(Socket, Key, Value, Width).

%% @spec putnr(Socket::port(), 
%%             Key::key(), 
%%             Value::value_or_num()) -> ok
%%
%% @doc 
%% Put a key/value pair to the remote database and do not wait for a response.
%% @end
putnr(Socket, Key, Value) when is_integer(Value), Value < 4294967296 ->
    putnr(Socket, Key, <<Value:32>>);
putnr(Socket, Key, Value) when is_float(Value) ->
    putnr(Socket, Key, <<Value:64/float>>);
putnr(Socket, Key, Value) ->
    ?T2(?PUTNR),
    ok.

%% @spec putnr(Socket::port(), 
%%             Key::key(), 
%%             Value::value_or_num(),
%%             endian()) -> ok
%%
%% @doc 
%% Put a key/value pair to the remote database and do not wait for a response.
%% The fourth parameter will decide how numbers are encoded for the remote
%% database.
%% @end
putnr(Socket, Key, Value, big) ->
    putnr(Socket, Key, Value);
putnr(Socket, Key, Value, little) when is_integer(Value), Value < 4294967296 ->
    putnr(Socket, Key, <<Value:32/little>>);
putnr(Socket, Key, Value, little) when is_float(Value) ->
    putnr(Socket, Key, <<Value:64/little-float>>);
putnr(Socket, Key, Value, little) ->
    putnr(Socket, Key, Value).

%% @spec out(Socket::port(), 
%%           Key::key()) -> ok | error()
%%
%% @doc 
%% Remove a key from the remote database.  Will return an error if Key is
%% not in the database.
%% @end
out(Socket, Key) ->
    ?T1(?OUT),
    ?R_SUCCESS.

%% @spec get(Socket::port(), 
%%           Key::key()) -> binary() | error()
%%
%% @doc Get the value for a given key
get(Socket, Key) ->
    ?T1(?GET),
    ?R_SIZE_DATA.

%% @spec mget(Socket::port(),
%%            KeyList::keylist()) -> [{Key::binary(), Value::binary()}] | error()
%%
%% @doc Get the values for a list of keys
mget(Socket, KeyList) when is_list(KeyList) ->
    gen_tcp:send(Socket, [<<?MGET:16>>, 
			  <<(length(KeyList)):32>>, 
			  [[<<(iolist_size(Key)):32>>, Key] || Key <- KeyList]
			 ]),
    tyrant_response(Socket, fun recv_count_4tuple/2).

%% @spec vsiz(Socket::port(),
%%            Key::key()) -> integer()
%%
%% @doc Get the size of the value for a given key.
vsiz(Socket, Key) ->
    ?T1(?VSIZ),
    ?R_INT32.

%% @spec iterinit(Socket::port()) -> ok | error()
%%
%% @doc Start iteration protocol.  WARNING: The tyrant iteration protocol has no
%% concurrency controls whatsoever, so if multiple clients try to do iteration
%% they will stomp all over each other!
%% @end
iterinit(Socket) ->
    ?T0(?ITERINIT),
    ?R_SUCCESS.

%% @spec iternext(Socket::port()) -> Key::binary() | error()
%%
%% @doc Get the next key/value pair in the iteration protocol
iternext(Socket) ->
    ?T0(?ITERNEXT),
    ?R_SIZE_DATA.

%% @spec fwmkeys(Socket::port(),
%%               Prefix::iolist(),
%%               MaxKeys::integer()) -> [binary()]
%%
%% @doc Return a number of keys that match a given prefix.
fwmkeys(Socket, Prefix, MaxKeys) when is_integer(MaxKeys) ->
    gen_tcp:send(Socket, [<<?FWMKEYS:16>>, 
			  <<(iolist_size(Prefix)):32>>, 
			  <<MaxKeys:32>>, Prefix]),
    tyrant_response(Socket, fun recv_count_2tuple/2).

%% @spec addint(Socket::port(),
%%              Key::key(),
%%              Int::integer()) -> integer() | error()
%%
%% @doc Add an integer value to the existing value of a key, returns new value
addint(Socket, Key, Int) when is_integer(Int) ->
    gen_tcp:send(Socket, [<<?ADDINT:16>>, <<(iolist_size(Key)):32>>, <<Int:32>>, Key]),
    ?R_INT32.

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Double::float()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc Add a float to the existing value of a key, returns new value.
adddouble(Socket, Key, Double) when is_float(Double) ->
    IntPart = trunc(Double),
    FracPart = trunc((Double - IntPart) * 1.0e12),
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>, 
			  <<(iolist_size(Key)):32>>,
			  <<IntPart:64>>, 
			  <<FracPart:64>>,
			  Key]),
    tyrant_response(Socket, fun recv_size64_size64/2).

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Double::float() | IntPart::integer(),
%%                 endian() | FracPart::integer()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc 
%% Add a float to the existing value of a key, returns new value. The byte-order
%% of the remote database is specified by the last parameter.
%% @end
adddouble(Socket, Key, Double, big) ->
    adddouble(Socket, Key, Double);
adddouble(Socket, Key, Double, little) when is_float(Double) ->
    IntPart = trunc(Double),
    FracPart = trunc((Double - IntPart) * 1.0e12),
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>, 
			  <<(iolist_size(Key)):32>>,
			  <<IntPart:64/little>>, 
			  <<FracPart:64/little>>,
			  Key]),
    tyrant_response(Socket, fun recv_size64_size64/2);
%% Need to stuff this one in here because the arity is 4
adddouble(Socket, Key, IntPart, FracPart) when is_integer(IntPart), is_integer(FracPart) ->
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>, 
			  <<(iolist_size(Key)):32>>,
			  <<IntPart:64>>, 
			  <<FracPart:64>>,
			  Key]),
    tyrant_response(Socket, fun recv_size64_size64/2).

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Integral::integer(),
%%                 Fractional::integer(),
%%                 endian()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc 
%% The raw adddouble function for those who need a bit more control on float adds
%% The last parameter determines remote database byte-order.
%% @end
adddouble(Socket, Key, IntPart, FracPart, little) when is_integer(IntPart), is_integer(FracPart) ->
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>, 
			  <<(iolist_size(Key)):32>>, 
			  <<IntPart:64/little>>, 
			  <<FracPart:64/little>>,
			  Key]),
    tyrant_response(Socket, fun recv_size64_size64/2);
adddouble(Socket, Key, IntPart, FracPart, big) when is_integer(IntPart), is_integer(FracPart) ->
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>, 
			  <<(iolist_size(Key)):32>>,
			  <<IntPart:64>>, 
			  <<FracPart:64>>,
			  Key]),
    tyrant_response(Socket, fun recv_size64_size64/2).

%% @spec sync(Socket::port()) -> ok | error()
%%
%% @doc Call sync() on the remote database
sync(Socket) ->
    ?T0(?SYNC),
    ?R_SUCCESS.

%% @spec vanish(Socket::port()) -> ok | error()
%%
%% @doc Remove all records from the remote database.
vanish(Socket) ->
    ?T0(?VANISH),
    ?R_SUCCESS.

%% @spec optimize(Socket::port(),
%%                iolist()) -> ok | error()
%%
%% @doc Change the remote database tuning parameters.  The second parameter
%%      should be a list of the database tuning parameters that will be applied
%%      at the remote end (e.g. "#bnum=1000000#opts=ld").
optimize(Socket, Key) ->
    ?T1(?OPTIMIZE), % Using 'Key' so that the macro binds properly...
    ?R_SUCCESS.

%% @spec rnum(Socket::port()) -> integer() | error()
%%
%% @doc Get the number of records in the remote database.
rnum(Socket) ->
    ?T0(?RNUM),
    ?R_INT64.

%% @spec size(Socket::port()) -> integer() | error()
%%
%% @doc Get the size in bytes of the remote database.
size(Socket) ->
    ?T0(?SIZE),
    ?R_INT64.

%% @spec stat(Socket::port()) -> proplist() | error()
%%
%% @doc Get the status string of a remote database.
stat(Socket) ->
    ?T0(?STAT),
    StatString = ?R_SIZE_DATA,
    case StatString of
	{error, Reason} ->
	    {error, Reason};
	GoodStat ->
	    stat_to_proplist(GoodStat)
    end.

stat_to_proplist(StatBin) ->
    stat_to_proplist(string:tokens(binary_to_list(StatBin), "\n\t"), []).

stat_to_proplist([], Acc) ->
    Acc;
stat_to_proplist([H1, H2 | T], Acc) ->
    stat_to_proplist(T, [{list_to_atom(H1), H2} | Acc]).

%% @spec copy(Socket::port(), 
%%            iolist()) -> ok | error()
%%
%% @doc Make a copy of the database file of the remote database.
copy(Socket, Key) when is_binary(Key) ->
    ?T1(?COPY), % Using 'Key' so that the macro binds properly...
    ?R_SUCCESS.

%% @spec restore(Socket::port(), 
%%               PathName::iolist(), 
%%               TimeStamp::integer,
%%               Options::) -> ok | error()
%%
%% @doc Restore the database to a particular point in time from the update log.
restore(Socket, PathName, TimeStamp) ->
    gen_tcp:send(Socket, [<<?RESTORE:16>>, 
			  <<(iolist_size(PathName)):32>>,
			  <<TimeStamp:64>>,
			  <<0:32>>,
			  PathName]),
    ?R_SUCCESS.

%% @spec restore_with_check(Socket::port(), 
%%                          PathName::iolist(), 
%%                          TimeStamp::integer) -> ok | error()
%%
%% @doc Restore the database to a particular point in time from the update log and
%%      perform a consistency check
%% @end
restore_with_check(Socket, PathName, TimeStamp) ->
    gen_tcp:send(Socket, [<<?RESTORE:16>>, 
			  <<(iolist_size(PathName)):32>>,
			  <<TimeStamp:64>>,
			  <<1:32>>,
			  PathName]),
    ?R_SUCCESS.    

%% @spec setmst(Socket::port(), 
%%              HostName::iolist(), 
%%              Port::integer) -> ok | error()
%%
%% @doc Set the replication master of a remote database server.
setmst(Socket, HostName, Port) when is_integer(Port) ->
    gen_tcp:send(Socket, [<<?SETMST:16>>, 
			  <<(iolist_size(HostName)):32>>, 
			  <<Port:32>>, HostName]),
    ?R_SUCCESS.

%% @spec repl(Socket::port(),
%%            TimeStamp::integer(),
%%            Sid::integer())
%%
%% @doc Initiate master->slave replication to the server id provided starting at a
%%      given timestamp.
%% repl(Socket, TimeStamp, Sid) ->
%%     gen_tcp:send(Socket, [<<?SETMST:16>>, 
%% 			  <<TimeStamp:64>>, 
%% 			  <<Sid:32>>]),
%%     ?R_SUCCESS.

%% @spec misc(Socket::port(),
%%            Func::iolist(),
%%            Args::arglist()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc
%% Tyrant misc() call that writes to the update logs
%% All database types support putlist, outlist, and getlist.
%%    putlist -> store records, Args is list of sequential keys and values, returns []
%%    outlist -> remove records, Args is list of keys, returns []
%%    getlist -> retrieve records, args is list of keys, returns list of values
%% Table database supports setindex, search, and genuid.
%%    setindex -> set the column index, Arg is name of col and type of col data, returns success val
%%    search -> run a search on the columns, returns list of values
%%    genuid -> generate unique ID number, returns integer
%% @end
misc(Socket, Func, Args) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<0:32>>, 
			  <<(length(Args)):32>>, 
			  Func,
			  misc_arg_encode(big, Args)
			 ]),
    tyrant_response(Socket, fun recv_count_2tuple/2);
misc(Socket, Func, _Args) ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<0:32>>, 
			  <<0:32>>, 
			  Func]),
    tyrant_response(Socket, fun recv_count_2tuple/2).

%% @spec misc(Socket::port(),
%%            Func::iolist(),
%%            Args::arglist(),
%%            endian()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc
%% Tyrant misc() call that writes to the update logs with a specific
%% byte-order for encoding
%% All database types support putlist, outlist, and getlist.
%%    putlist -> store records, Args is list of sequential keys and values, returns []
%%    outlist -> remove records, Args is list of keys, returns []
%%    getlist -> retrieve records, args is list of keys, returns list of values
%% Table database supports setindex, search, and genuid.
%%    setindex -> set the column index, Arg is name of col and type of col data, returns success val
%%    search -> run a search on the columns, returns list of values
%%    genuid -> generate unique ID number, returns integer
%% @end
misc(Socket, Func, Args, big) ->
    misc(Socket, Func, Args);
misc(Socket, Func, Args, little) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<0:32>>, 
			  <<(length(Args)):32>>, 
			  Func,
			  misc_arg_encode(little, Args)
			 ]),
    tyrant_response(Socket, fun recv_count_2tuple/2);
misc(Socket, Func, Args, _Endian) ->
    misc(Socket, Func, Args).

%% @spec misc_no_update(Socket::port(),
%%                      Func::iolist(),
%%                      Args::arglist()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc Tyrant misc() call that does not write to the update logs
misc_no_update(Socket, Func, Args) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<1:32>>, 
			  <<(length(Args)):32>>,
			  Func,
			  misc_arg_encode(big, Args)
			 ]),
    tyrant_response(Socket, fun recv_count_2tuple/2);
misc_no_update(Socket, Func, _Args) ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<1:32>>, 
			  <<0:32>>, 
			  Func]),
    tyrant_response(Socket, fun recv_count_2tuple/2).

%% @spec misc_no_update(Socket::port(),
%%                      Func::iolist(),
%%                      Args::arglist(),
%%                      endian()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc Tyrant misc() call that does not write to the update logs.
misc_no_update(Socket, Func, Args, big) ->
    misc_no_update(Socket, Func, Args);
misc_no_update(Socket, Func, Args, little) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<1:32>>, 
			  <<(length(Args)):32>>,
			  Func,
			  misc_arg_encode(little, Args)
			 ]),
    tyrant_response(Socket, fun recv_count_2tuple/2);
misc_no_update(Socket, Func, Args, little) ->
    misc_no_update(Socket, Func, Args).

%% Encoding helper for misc() that tries to keep integers in the
%% proper form for the remote database.
misc_arg_encode(Endian, ArgList) ->
    misc_arg_encode(Endian, ArgList, []).

misc_arg_encode(_Endian, [], ArgList) ->
    lists:reverse(ArgList);
misc_arg_encode(big, [Arg | Tail], ArgList) when is_integer(Arg), Arg < 4294967296 ->
    misc_arg_encode(big, Tail, [[<<4:32>>, <<Arg:32>>] | ArgList]);
misc_arg_encode(little, [Arg | Tail], ArgList) when is_integer(Arg), Arg < 4294967296 ->
    misc_arg_encode(little, Tail, [[<<4:32>>, <<Arg:32/little>>] | ArgList]);
misc_arg_encode(big, [Arg | Tail], ArgList) when is_float(Arg) ->
    misc_arg_encode(big, Tail, [[<<8:32>>, <<Arg:64/float>>] | ArgList]);
misc_arg_encode(little, [Arg | Tail], ArgList) when is_float(Arg) ->
    misc_arg_encode(little, Tail, [[<<8:32>>, <<Arg:64/float-little>>] | ArgList]);
misc_arg_encode(Endian, [Arg | Tail], ArgList) ->
    misc_arg_encode(Endian, Tail, [[<<(iolist_size(Arg)):32>>, Arg] | ArgList]).

%% @spec ext(Socket::port(),
%%            Func::iolist(),
%%            Opts::proplist(),
%%            Key::iolist(),
%%            Value::iolist()) -> ok | error()
%%
%% @doc Call a function defined by the Tyrant script language extensions.
ext(Socket, Func, Opts, Key, Value) ->
    %% TODO: Opts needs to be parsed.  Probably as a proplist [record_lock, global_lock, neither...]
    gen_tcp:send(Socket, [<<?EXT:16>>, <<(iolist_size(Func)):32>>, <<Opts:32>>, 
			  <<(iolist_size(Key)):32>>, <<(iolist_size(Value)):32>>, 
			  Func, Key, Value]),
    ?R_SUCCESS.

%%====================================================================
%% Handle response from the server
%%====================================================================

tyrant_response(Socket, ResponseHandler) ->
    receive
	{tcp, Socket, <<1:8, _Rest/binary>>} ->
	    {error, invalid_operation};
	{tcp, Socket, <<2:8, _Rest/binary>>} ->
	    {error, no_host_found};
	{tcp, Socket, <<3:8, _Rest/binary>>} ->
	    {error, connection_refused};
	{tcp, Socket, <<4:8, _Rest/binary>>} ->
	    {error, send_error};
	{tcp, Socket, <<5:8, _Rest/binary>>} ->
	    {error, recv_error};
	{tcp, Socket, <<6:8, _Rest/binary>>} ->
	    {error, existing_record};
	{tcp, Socket, <<7:8, _Rest/binary>>} ->
	    {error, no_such_record};
        {tcp, Socket, <<ErrorCode:8, _Rest/binary>>} when ErrorCode =/= 0 ->
	    {error, ErrorCode};
        {tcp_closed, Socket} -> 
	    {error, conn_closed};
        {tcp_error, Socket, _Reason} -> 
	    {error, conn_error};
        Data -> 
	    ResponseHandler(Socket, Data)
    after ?TIMEOUT -> 
	    {error, timeout}
    end.

%% receive 8-bit success flag
recv_success(_Socket, {tcp, _, <<0:8>>}) -> 
    ok.
 
%% receive 8-bit success flag + 32-bit int (endianness determined by remote database)
recv_size(_Socket, {tcp, _, <<0:8, ValSize:32>>}) ->
    ValSize.
 
%% receive 8-bit success flag + 64-bit int
recv_size64(_Socket, {tcp, _, <<0:8, ValSize:64>>}) -> 
    ValSize.
 
%% receive 8-bit success flag + 64-bit int + 64-bit int
recv_size64_size64(_Socket, {tcp, _, <<0:8, V1:64, V2:64>>}) -> 
    {V1, V2}.
 
%% receive 8-bit success flag + length1 + data1
recv_size_data(Socket, Data) ->
    case Data of
        {tcp, _, <<0:8, Length:32, Rest/binary>>} ->
            {Value, <<>>} = recv_until(Socket, Rest, Length),
            Value
    end.

%% receive 8-bit success flag + count + (length1, length2, data1, data2)*count
recv_count_4tuple(Socket, Data) ->
    case Data of
        {tcp, _, <<0:8, 0:32, _Rest/binary>>} ->
            [];
        {tcp, _, <<0:8, RecCnt:32, Rest/binary>>} ->
            {KVS, _} = lists:mapfoldl(
                            fun(_N, Acc) ->
                                <<KeySize:32, ValSize:32, Bin/binary>> = Acc,
                                {Key, Rest1} = recv_until(Socket, Bin, KeySize),
                                {Value, Rest2} = recv_until(Socket, Rest1, ValSize),
                                {{Key, Value}, Rest2}
                            end, 
                            Rest, lists:seq(1, RecCnt)
                        ),
            KVS
    end.

%% receive 8-bit success flag + count + (length1, data1)*count
recv_count_2tuple(Socket, Data) ->
    case Data of
        {tcp, _, <<0:8, 0:32, _Rest/binary>>} ->
	    [];
        {tcp, _, <<0:8, Cnt:32, Rest/binary>>} ->
            {Keys, _} = lists:mapfoldl(
                            fun(_N, Acc) ->
                                <<KeySize:32, Bin/binary>> = Acc,
                                recv_until(Socket, Bin, KeySize)
                            end,
                            Rest, lists:seq(1, Cnt)
                        ),
            Keys
    end.
 
%% receive length-delimited data that may require multiple pulls from the socket
recv_until(Socket, Bin, ReqLength) when byte_size(Bin) < ReqLength ->
    receive
        {tcp, Socket, Data} ->
            Combined = <<Bin/binary, Data/binary>>,
            recv_until(Socket, Combined, ReqLength);
        {tcp_closed, Socket} -> 
	    {error, conn_closed};
	{error, closed} ->
	    {error, conn_closed}
    after ?TIMEOUT -> 
	    {error, timeout}
    end;    
recv_until(_Socket, Bin, ReqLength) when byte_size(Bin) =:= ReqLength ->
    {Bin, <<>>};
recv_until(_Socket, Bin, ReqLength) when byte_size(Bin) > ReqLength ->
    <<Required:ReqLength/binary, Rest/binary>> = Bin,
    {Required, Rest}.


%% Some standard types for edoc
%%
%% @type key() = iolist()
%% @type value() = iolist()
%% @type value_or_num() = iolist() | integer() | float()
%% @type keylist() = [key()]
%% @type error() = {error, term()}
%% @type endian() = little | big
