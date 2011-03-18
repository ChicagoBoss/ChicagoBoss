%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.

-module(pgsql).

-export([connect/2, connect/3, connect/4, close/1]).
-export([get_parameter/2, squery/2, equery/2, equery/3]).
-export([parse/2, parse/3, parse/4, describe/2, describe/3]).
-export([bind/3, bind/4, execute/2, execute/3, execute/4]).
-export([close/2, close/3, sync/1]).
-export([with_transaction/2]).

-include("pgsql.hrl").

%% -- client interface --

connect(Host, Opts) ->
    connect(Host, os:getenv("USER"), "", Opts).

connect(Host, Username, Opts) ->
    connect(Host, Username, "", Opts).

connect(Host, Username, Password, Opts) ->
    {ok, C} = pgsql_connection:start_link(),
    pgsql_connection:connect(C, Host, Username, Password, Opts).

close(C) when is_pid(C) ->
    catch pgsql_connection:stop(C),
    ok.

get_parameter(C, Name) ->
    pgsql_connection:get_parameter(C, Name).

squery(C, Sql) ->
    ok = pgsql_connection:squery(C, Sql),
    case receive_results(C, []) of
        [Result] -> Result;
        Results  -> Results
    end.

equery(C, Sql) ->
    equery(C, Sql, []).

equery(C, Sql, Parameters) ->
    case pgsql_connection:parse(C, "", Sql, []) of
        {ok, #statement{types = Types} = S} ->
            Typed_Parameters = lists:zip(Types, Parameters),
            ok = pgsql_connection:equery(C, S, Typed_Parameters),
            receive_result(C, undefined);
        Error ->
            Error
    end.

%% parse

parse(C, Sql) ->
    parse(C, "", Sql, []).

parse(C, Sql, Types) ->
    parse(C, "", Sql, Types).

parse(C, Name, Sql, Types) ->
    pgsql_connection:parse(C, Name, Sql, Types).

%% bind

bind(C, Statement, Parameters) ->
    bind(C, Statement, "", Parameters).

bind(C, Statement, PortalName, Parameters) ->
    pgsql_connection:bind(C, Statement, PortalName, Parameters).

%% execute

execute(C, S) ->
    execute(C, S, "", 0).

execute(C, S, N) ->
    execute(C, S, "", N).

execute(C, S, PortalName, N) ->
    pgsql_connection:execute(C, S, PortalName, N),
    receive_extended_result(C).

%% statement/portal functions

describe(C, #statement{name = Name}) ->
    pgsql_connection:describe(C, statement, Name).

describe(C, Type, Name) ->
    pgsql_connection:describe(C, Type, Name).

close(C, #statement{name = Name}) ->
    pgsql_connection:close(C, statement, Name).

close(C, Type, Name) ->
    pgsql_connection:close(C, Type, Name).

sync(C) ->
    pgsql_connection:sync(C).

%% misc helper functions
with_transaction(C, F) ->
    try {ok, [], []} = squery(C, "BEGIN"),
        R = F(C),
        {ok, [], []} = squery(C, "COMMIT"),
        R
    catch
        _:Why ->
            squery(C, "ROLLBACK"),
            {rollback, Why}
    end.

%% -- internal functions --

receive_result(C, Result) ->
    try receive_result(C, [], []) of
        done    -> Result;
        R       -> receive_result(C, R)
    catch
        throw:E -> E
    end.

receive_results(C, Results) ->
    try receive_result(C, [], []) of
        done    -> lists:reverse(Results);
        R       -> receive_results(C, [R | Results])
    catch
        throw:E -> E
    end.

receive_result(C, Cols, Rows) ->
    receive
        {pgsql, C, {columns, Cols2}} ->
            receive_result(C, Cols2, Rows);
        {pgsql, C, {data, Row}} ->
            receive_result(C, Cols, [Row | Rows]);
        {pgsql, C, {error, _E} = Error} ->
            Error;
        {pgsql, C, {complete, {_Type, Count}}} ->
            case Rows of
                [] -> {ok, Count};
                _L -> {ok, Count, Cols, lists:reverse(Rows)}
            end;
        {pgsql, C, {complete, _Type}} ->
            {ok, Cols, lists:reverse(Rows)};
        {pgsql, C, done} ->
            done;
        {pgsql, C, timeout} ->
            throw({error, timeout});
        {'EXIT', C, _Reason} ->
            throw({error, closed})
    end.

receive_extended_result(C)->
    receive_extended_result(C, []).

receive_extended_result(C, Rows) ->
    receive
        {pgsql, C, {data, Row}} ->
            receive_extended_result(C, [Row | Rows]);
        {pgsql, C, {error, _E} = Error} ->
            Error;
        {pgsql, C, suspended} ->
            {partial, lists:reverse(Rows)};
        {pgsql, C, {complete, {_Type, Count}}} ->
            case Rows of
                [] -> {ok, Count};
                _L -> {ok, Count, lists:reverse(Rows)}
            end;
        {pgsql, C, {complete, _Type}} ->
            {ok, lists:reverse(Rows)};
        {pgsql, C, timeout} ->
            {error, timeout};
        {'EXIT', C, _Reason} ->
            {error, closed}
    end.
