%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.

-module(pgsql_connection).

-behavior(gen_fsm).

-export([start_link/0, stop/1, connect/5, get_parameter/2]).
-export([squery/2, equery/3]).
-export([parse/4, bind/4, execute/4, describe/3]).
-export([close/3, sync/1]).

-export([init/1, handle_event/3, handle_sync_event/4]).
-export([handle_info/3, terminate/3, code_change/4]).

-export([startup/3, auth/2, initializing/2, ready/2, ready/3]).
-export([querying/2, parsing/2, binding/2, describing/2]).
-export([executing/2, closing/2, synchronizing/2, timeout/2]).

-include("pgsql.hrl").

-record(state, {
          reader,
          sock,
          timeout,
          parameters = [],
          reply,
          reply_to,
          async,
          backend,
          statement,
          txstatus}).

-define(int16, 1/big-signed-unit:16).
-define(int32, 1/big-signed-unit:32).

%% -- client interface --

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

stop(C) ->
    gen_fsm:send_all_state_event(C, stop).

connect(C, Host, Username, Password, Opts) ->
    gen_fsm:sync_send_event(C, {connect, Host, Username, Password, Opts}, infinity).

get_parameter(C, Name) ->
    gen_fsm:sync_send_event(C, {get_parameter, to_binary(Name)}).

squery(C, Sql) ->
    gen_fsm:sync_send_event(C, {squery, Sql}, infinity).

equery(C, Statement, Parameters) ->
    gen_fsm:sync_send_event(C, {equery, Statement, Parameters}, infinity).

parse(C, Name, Sql, Types) ->
    gen_fsm:sync_send_event(C, {parse, Name, Sql, Types}, infinity).

bind(C, Statement, PortalName, Parameters) ->
    gen_fsm:sync_send_event(C, {bind, Statement, PortalName, Parameters}, infinity).

execute(C, Statement, PortalName, MaxRows) ->
    gen_fsm:sync_send_event(C, {execute, Statement, PortalName, MaxRows}, infinity).

describe(C, Type, Name) ->
    gen_fsm:sync_send_event(C, {describe, Type, Name}, infinity).

close(C, Type, Name) ->
    gen_fsm:sync_send_event(C, {close, Type, Name}, infinity).

sync(C) ->
    gen_fsm:sync_send_event(C, sync, infinity).

%% -- gen_fsm implementation --

init([]) ->
    process_flag(trap_exit, true),
    {ok, startup, #state{}}.

handle_event({notice, _Notice} = Msg, State_Name, State) ->
    notify_async(State, Msg),
    {next_state, State_Name, State};

handle_event({notification, _Channel, _Pid, _Payload} = Msg, State_Name, State) ->
    notify_async(State, Msg),
    {next_state, State_Name, State};

handle_event({parameter_status, Name, Value}, State_Name, State) ->
    Parameters2 = lists:keystore(Name, 1, State#state.parameters, {Name, Value}),
    {next_state, State_Name, State#state{parameters = Parameters2}};

handle_event(stop, _State_Name, State) ->
    {stop, normal, State};

handle_event(Event, _State_Name, State) ->
    {stop, {unsupported_event, Event}, State}.

handle_sync_event(Event, _From, _State_Name, State) ->
    {stop, {unsupported_sync_event, Event}, State}.

handle_info({'EXIT', Pid, Reason}, _State_Name, State = #state{sock = Pid}) ->
    {stop, Reason, State};

handle_info(Info, _State_Name, State) ->
    {stop, {unsupported_info, Info}, State}.

terminate(_Reason, _State_Name, State = #state{sock = Sock})
  when Sock =/= undefined ->
    send(State, $X, []);

terminate(_Reason, _State_Name, _State) ->
    ok.

code_change(_Old_Vsn, State_Name, State, _Extra) ->
    {ok, State_Name, State}.

%% -- states --

startup({connect, Host, Username, Password, Opts}, From, State) ->
    Timeout = proplists:get_value(timeout, Opts, 5000),
    Async   = proplists:get_value(async, Opts, undefined),
    case pgsql_sock:start_link(self(), Host, Username, Opts) of
        {ok, Sock} ->
            put(username, Username),
            put(password, Password),
            State2 = State#state{
                       sock     = Sock,
                       timeout  = Timeout,
                       reply_to = From,
                       async    = Async},
            {next_state, auth, State2, Timeout};
        Error ->
            {stop, normal, Error, State}
    end.

%% AuthenticationOk
auth({$R, <<0:?int32>>}, State) ->
    #state{timeout = Timeout} = State,
    {next_state, initializing, State, Timeout};

%% AuthenticationCleartextPassword
auth({$R, <<3:?int32>>}, State) ->
    #state{timeout = Timeout} = State,
    send(State, $p, [get(password), 0]),
    {next_state, auth, State, Timeout};

%% AuthenticationMD5Password
auth({$R, <<5:?int32, Salt:4/binary>>}, State) ->
    #state{timeout = Timeout} = State,
    Digest1 = hex(erlang:md5([get(password), get(username)])),
    Str = ["md5", hex(erlang:md5([Digest1, Salt])), 0],
    send(State, $p, Str),
    {next_state, auth, State, Timeout};

auth({$R, <<M:?int32, _/binary>>}, State) ->
    case M of
        2 -> Method = kerberosV5;
        4 -> Method = crypt;
        6 -> Method = scm;
        7 -> Method = gss;
        8 -> Method = sspi;
        _ -> Method = unknown
    end,
    Error = {error, {unsupported_auth_method, Method}},
    gen_fsm:reply(State#state.reply_to, Error),
    {stop, normal, State};

%% ErrorResponse
auth({error, E}, State) ->
    case E#error.code of
        <<"28000">> -> Why = invalid_authorization_specification;
        <<"28P01">> -> Why = invalid_password;
        Any         -> Why = Any
    end,
    gen_fsm:reply(State#state.reply_to, {error, Why}),
    {stop, normal, State};

auth(timeout, State) ->
    gen_fsm:reply(State#state.reply_to, {error, timeout}),
    {stop, normal, State}.

%% BackendKeyData
initializing({$K, <<Pid:?int32, Key:?int32>>}, State) ->
    #state{timeout = Timeout} = State,
    State2 = State#state{backend = {Pid, Key}},
    {next_state, initializing, State2, Timeout};

%% ErrorResponse
initializing({error, E}, State) ->
    case E#error.code of
        <<"28000">> -> Why = invalid_authorization_specification;
        Any         -> Why = Any
    end,
    gen_fsm:reply(State#state.reply_to, {error, Why}),
    {stop, normal, State};

initializing(timeout, State) ->
    gen_fsm:reply(State#state.reply_to, {error, timeout}),
    {stop, normal, State};

%% ReadyForQuery
initializing({$Z, <<Status:8>>}, State) ->
    #state{parameters = Parameters, reply_to = Reply_To} = State,
    erase(username),
    erase(password),
    case lists:keysearch(<<"integer_datetimes">>, 1, Parameters) of
        {value, {_, <<"on">>}}  -> put(datetime_mod, pgsql_idatetime);
        {value, {_, <<"off">>}} -> put(datetime_mod, pgsql_fdatetime)
    end,
    gen_fsm:reply(Reply_To, {ok, self()}),
    {next_state, ready, State#state{txstatus = Status}}.

ready(_Msg, State) ->
    {next_state, ready, State}.

%% execute simple query
ready({squery, Sql}, From, State) ->
    #state{timeout = Timeout} = State,
    send(State, $Q, [Sql, 0]),
    State2 = State#state{statement = #statement{}, reply_to = From},
    {reply, ok, querying, State2, Timeout};

%% execute extended query
ready({equery, Statement, Parameters}, From, State) ->
    #state{timeout = Timeout} = State,
    #statement{name = StatementName, columns = Columns} = Statement,
    Bin1 = encode_parameters(Parameters),
    Bin2 = encode_formats(Columns),
    send(State, $B, ["", 0, StatementName, 0, Bin1, Bin2]),
    send(State, $E, ["", 0, <<0:?int32>>]),
    send(State, $C, [$S, "", 0]),
    send(State, $S, []),
    State2 = State#state{statement = Statement, reply_to = From},
    {reply, ok, querying, State2, Timeout};

ready({get_parameter, Name}, _From, State) ->
    case lists:keysearch(Name, 1, State#state.parameters) of
        {value, {Name, Value}} -> Value;
        false                  -> Value = undefined
    end,
    {reply, {ok, Value}, ready, State};

ready({parse, Name, Sql, Types}, From, State) ->
    #state{timeout = Timeout} = State,
    Bin = encode_types(Types),
    send(State, $P, [Name, 0, Sql, 0, Bin]),
    send(State, $D, [$S, Name, 0]),
    send(State, $H, []),
    S = #statement{name = Name},
    State2 = State#state{statement = S, reply_to = From},
    {next_state, parsing, State2, Timeout};

ready({bind, Statement, PortalName, Parameters}, From, State) ->
    #state{timeout = Timeout} = State,
    #statement{name = StatementName, columns = Columns, types = Types} = Statement,
    Typed_Parameters = lists:zip(Types, Parameters),
    Bin1 = encode_parameters(Typed_Parameters),
    Bin2 = encode_formats(Columns),
    send(State, $B, [PortalName, 0, StatementName, 0, Bin1, Bin2]),
    send(State, $H, []),
    State2 = State#state{statement = Statement, reply_to = From},
    {next_state, binding, State2, Timeout};

ready({execute, Statement, PortalName, MaxRows}, From, State) ->
    #state{timeout = Timeout} = State,
    send(State, $E, [PortalName, 0, <<MaxRows:?int32>>]),
    send(State, $H, []),
    State2 = State#state{statement = Statement, reply_to = From},
    {reply, ok, executing, State2, Timeout};

ready({describe, Type, Name}, From, State) ->
    #state{timeout = Timeout} = State,
    case Type of
        statement -> Type2 = $S;
        portal    -> Type2 = $P
    end,
    send(State, $D, [Type2, Name, 0]),
    send(State, $H, []),
    {next_state, describing, State#state{reply_to = From}, Timeout};

ready({close, Type, Name}, From, State) ->
    #state{timeout = Timeout} = State,
    case Type of
        statement -> Type2 = $S;
        portal    -> Type2 = $P
    end,
    send(State, $C, [Type2, Name, 0]),
    send(State, $H, []),
    {next_state, closing, State#state{reply_to = From}, Timeout};

ready(sync, From, State) ->
    #state{timeout = Timeout} = State,
    send(State, $S, []),
    State2 = State#state{reply = ok, reply_to = From},
    {next_state, synchronizing, State2, Timeout}.

%% BindComplete
querying({$2, <<>>}, State) ->
    #state{timeout = Timeout, statement = #statement{columns = Columns}} = State,
    notify(State, {columns, Columns}),
    {next_state, querying, State, Timeout};

%% CloseComplete
querying({$3, <<>>}, State) ->
    #state{timeout = Timeout} = State,
    {next_state, querying, State, Timeout};

%% RowDescription
querying({$T, <<Count:?int16, Bin/binary>>}, State) ->
    #state{timeout = Timeout} = State,
    Columns = decode_columns(Count, Bin),
    S2 = (State#state.statement)#statement{columns = Columns},
    notify(State, {columns, Columns}),
    {next_state, querying, State#state{statement = S2}, Timeout};

%% DataRow
querying({$D, <<_Count:?int16, Bin/binary>>}, State) ->
    #state{timeout = Timeout, statement = #statement{columns = Columns}} = State,
    Data = decode_data(Columns, Bin),
    notify(State, {data, Data}),
    {next_state, querying, State, Timeout};

%% CommandComplete
querying({$C, Bin}, State) ->
    #state{timeout = Timeout} = State,
    Complete = decode_complete(Bin),
    notify(State, {complete, Complete}),
    {next_state, querying, State, Timeout};

%% EmptyQueryResponse
querying({$I, _Bin}, State) ->
    #state{timeout = Timeout} = State,
    notify(State, {complete, empty}),
    {next_state, querying, State, Timeout};

querying(timeout, State) ->
    #state{sock = Sock, timeout = Timeout, backend = {Pid, Key}} = State,
    pgsql_sock:cancel(Sock, Pid, Key),
    {next_state, timeout, State, Timeout};

%% ErrorResponse
querying({error, E}, State) ->
    #state{timeout = Timeout} = State,
    notify(State, {error, E}),
    {next_state, querying, State, Timeout};

%% ReadyForQuery
querying({$Z, <<_Status:8>>}, State) ->
    notify(State, done),
    {next_state, ready, State#state{reply_to = undefined}}.

%% ParseComplete
parsing({$1, <<>>}, State) ->
    #state{timeout = Timeout} = State,
    {next_state, describing, State, Timeout};

parsing(timeout, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, timeout},
    send(State, $S, []),
    {next_state, parsing, State#state{reply = Reply}, Timeout};

%% ErrorResponse
parsing({error, E}, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, E},
    send(State, $S, []),
    {next_state, parsing, State#state{reply = Reply}, Timeout};

%% ReadyForQuery
parsing({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

%% BindComplete
binding({$2, <<>>}, State) ->
    gen_fsm:reply(State#state.reply_to, ok),
    {next_state, ready, State};

binding(timeout, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, timeout},
    send(State, $S, []),
    {next_state, binding, State#state{reply = Reply}, Timeout};

%% ErrorResponse
binding({error, E}, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, E},
    send(State, $S, []),
    {next_state, binding, State#state{reply = Reply}, Timeout};

%% ReadyForQuery
binding({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

%% ParameterDescription
describing({$t, <<_Count:?int16, Bin/binary>>}, State) ->
    #state{timeout = Timeout} = State,
    Types = [pgsql_types:oid2type(Oid) || <<Oid:?int32>> <= Bin],
    S2 = (State#state.statement)#statement{types = Types},
    {next_state, describing, State#state{statement = S2}, Timeout};

%% RowDescription
describing({$T, <<Count:?int16, Bin/binary>>}, State) ->
    Columns = decode_columns(Count, Bin),
    Columns2 = [C#column{format = format(C#column.type)} || C <- Columns],
    S2 = (State#state.statement)#statement{columns = Columns2},
    gen_fsm:reply(State#state.reply_to, {ok, S2}),
    {next_state, ready, State};

%% NoData
describing({$n, <<>>}, State) ->
    S2 = (State#state.statement)#statement{columns = []},
    gen_fsm:reply(State#state.reply_to, {ok, S2}),
    {next_state, ready, State};

describing(timeout, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, timeout},
    send(State, $S, []),
    {next_state, describing, State#state{reply = Reply}, Timeout};

%% ErrorResponse
describing({error, E}, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, E},
    send(State, $S, []),
    {next_state, describing, State#state{reply = Reply}, Timeout};

%% ReadyForQuery
describing({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

%% DataRow
executing({$D, <<_Count:?int16, Bin/binary>>}, State) ->
    #state{timeout = Timeout, statement = #statement{columns = Columns}} = State,
    Data = decode_data(Columns, Bin),
    notify(State, {data, Data}),
    {next_state, executing, State, Timeout};

%% PortalSuspended
executing({$s, <<>>}, State) ->
    notify(State, suspended),
    {next_state, ready, State};

%% CommandComplete
executing({$C, Bin}, State) ->
    notify(State, {complete, decode_complete(Bin)}),
    {next_state, ready, State};

%% EmptyQueryResponse
executing({$I, _Bin}, State) ->
    notify(State, {complete, empty}),
    {next_state, ready, State};

executing(timeout, State) ->
    #state{sock = Sock, timeout = Timeout, backend = {Pid, Key}} = State,
    pgsql_sock:cancel(Sock, Pid, Key),
    send(State, $S, []),
    {next_state, timeout, State, Timeout};

%% ErrorResponse
executing({error, E}, State) ->
    #state{timeout = Timeout} = State,
    notify(State, {error, E}),
    {next_state, executing, State, Timeout}.

%% CloseComplete
closing({$3, <<>>}, State) ->
    gen_fsm:reply(State#state.reply_to, ok),
    {next_state, ready, State};

closing(timeout, State) ->
    gen_fsm:reply(State#state.reply_to, {error, timeout}),
    {next_state, ready, State};

%% ErrorResponse
closing({error, E}, State) ->
    Error = {error, E},
    gen_fsm:reply(State#state.reply_to, Error),
    {next_state, ready, State}.

%% ErrorResponse
synchronizing({error, E}, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, E},
    {next_state, synchronizing, State#state{reply = Reply}, Timeout};

synchronizing(timeout, State) ->
    #state{timeout = Timeout} = State,
    Reply = {error, timeout},
    {next_state, synchronizing, State#state{reply = Reply}, Timeout};

%% ReadyForQuery
synchronizing({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

timeout({$Z, <<Status:8>>}, State) ->
    notify(State, timeout),
    {next_state, ready, State#state{txstatus = Status}};

timeout(timeout, State) ->
    {stop, timeout, State};

%% ignore events that occur after timeout
timeout(_Event, State) ->
    #state{timeout = Timeout} = State,
    {next_state, timeout, State, Timeout}.

%% -- internal functions --

%% decode data
decode_data(Columns, Bin) ->
    decode_data(Columns, Bin, []).

decode_data([], _Bin, Acc) ->
    list_to_tuple(lists:reverse(Acc));
decode_data([_C | T], <<-1:?int32, Rest/binary>>, Acc) ->
    decode_data(T, Rest, [null | Acc]);
decode_data([C | T], <<Len:?int32, Value:Len/binary, Rest/binary>>, Acc) ->
    case C of
        #column{type = Type, format = 1}   -> Value2 = pgsql_binary:decode(Type, Value);
        #column{}                          -> Value2 = Value
    end,
    decode_data(T, Rest, [Value2 | Acc]).

%% decode column information
decode_columns(Count, Bin) ->
    decode_columns(Count, Bin, []).

decode_columns(0, _Bin, Acc) ->
    lists:reverse(Acc);
decode_columns(N, Bin, Acc) ->
    {Name, Rest} = pgsql_sock:decode_string(Bin),
    <<_Table_Oid:?int32, _Attrib_Num:?int16, Type_Oid:?int32,
     Size:?int16, Modifier:?int32, Format:?int16, Rest2/binary>> = Rest,
    Desc = #column{
      name     = Name,
      type     = pgsql_types:oid2type(Type_Oid),
      size     = Size,
      modifier = Modifier,
      format   = Format},
    decode_columns(N - 1, Rest2, [Desc | Acc]).

%% decode command complete msg
decode_complete(<<"SELECT", 0>>)        -> select;
decode_complete(<<"SELECT", _/binary>>) -> select;
decode_complete(<<"BEGIN", 0>>)         -> 'begin';
decode_complete(<<"ROLLBACK", 0>>)      -> rollback;
decode_complete(Bin) ->
    {Str, _} = pgsql_sock:decode_string(Bin),
    case string:tokens(binary_to_list(Str), " ") of
        ["INSERT", _Oid, Rows] -> {insert, list_to_integer(Rows)};
        ["UPDATE", Rows]       -> {update, list_to_integer(Rows)};
        ["DELETE", Rows]       -> {delete, list_to_integer(Rows)};
        ["MOVE", Rows]         -> {move, list_to_integer(Rows)};
        ["FETCH", Rows]        -> {fetch, list_to_integer(Rows)};
        [Type | _Rest]         -> pgsql_sock:lower_atom(Type)
    end.

%% encode types
encode_types(Types) ->
    encode_types(Types, 0, <<>>).

encode_types([], Count, Acc) ->
    <<Count:?int16, Acc/binary>>;

encode_types([Type | T], Count, Acc) ->
    case Type of
        undefined -> Oid = 0;
        _Any      -> Oid = pgsql_types:type2oid(Type)
    end,
    encode_types(T, Count + 1, <<Acc/binary, Oid:?int32>>).

%% encode column formats
encode_formats(Columns) ->
    encode_formats(Columns, 0, <<>>).

encode_formats([], Count, Acc) ->
    <<Count:?int16, Acc/binary>>;

encode_formats([#column{format = Format} | T], Count, Acc) ->
    encode_formats(T, Count + 1, <<Acc/binary, Format:?int16>>).

format(Type) ->
    case pgsql_binary:supports(Type) of
        true  -> 1;
        false -> 0
    end.

%% encode parameters
encode_parameters(Parameters) ->
    encode_parameters(Parameters, 0, <<>>, <<>>).

encode_parameters([], Count, Formats, Values) ->
    <<Count:?int16, Formats/binary, Count:?int16, Values/binary>>;

encode_parameters([P | T], Count, Formats, Values) ->
    {Format, Value} = encode_parameter(P),
    Formats2 = <<Formats/binary, Format:?int16>>,
    Values2 = <<Values/binary, Value/binary>>,
    encode_parameters(T, Count + 1, Formats2, Values2).

%% encode parameter

encode_parameter({Type, Value}) ->
    case pgsql_binary:encode(Type, Value) of
        Bin when is_binary(Bin) -> {1, Bin};
        {error, unsupported}    -> encode_parameter(Value)
    end;
encode_parameter(A) when is_atom(A)    -> {0, encode_list(atom_to_list(A))};
encode_parameter(B) when is_binary(B)  -> {0, <<(byte_size(B)):?int32, B/binary>>};
encode_parameter(I) when is_integer(I) -> {0, encode_list(integer_to_list(I))};
encode_parameter(F) when is_float(F)   -> {0, encode_list(float_to_list(F))};
encode_parameter(L) when is_list(L)    -> {0, encode_list(L)}.

encode_list(L) ->
    Bin = list_to_binary(L),
    <<(byte_size(Bin)):?int32, Bin/binary>>.

notify(#state{reply_to = {Pid, _Tag}}, Msg) ->
    Pid ! {pgsql, self(), Msg}.

notify_async(#state{async = Pid}, Msg) ->
    case is_pid(Pid) of
        true  -> Pid ! {pgsql, self(), Msg};
        false -> false
    end.

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L)   -> list_to_binary(L).

hex(Bin) ->
    HChar = fun(N) when N < 10 -> $0 + N;
               (N) when N < 16 -> $W + N
            end,
    <<<<(HChar(H)), (HChar(L))>> || <<H:4, L:4>> <= Bin>>.

%% send data to server

send(#state{sock = Sock}, Type, Data) ->
    pgsql_sock:send(Sock, Type, Data).
