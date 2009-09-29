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
%%% File:      medici.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright Copyright (c) 2009, Jim McCoy.  All Rights Reserved.
%%%
%%% @doc
%%% This module provides the primary API for interfacing with the
%%% medici application. These functions assume you are using the default 
%%% registered name for the service and that you know what sort of remote database
%%% you are talking to (e.g. don't make table-specific calls to a hash
%%% database.) If you need byte-order specific ops, want to register the
%%% controller with a different name, or want to run medici interfaces
%%% to multiple remote databases within the same erlang VM the you should
%%% update these or make your own versions to make the appropriate calls
%%% to the controller.
%%% @end

-module(medici).

%% Starting and stopping the app
-export([start/0, start/1, stop/0]).

%% Basic API exports
-export([put/2, putkeep/2, putcat/2, putshl/3, putnr/2, out/1, get/1, 
	 mget/1, vsiz/1, iterinit/0, iternext/0, fwmkeys/2, addint/2,
	 adddouble/2, adddouble/3, sync/0, vanish/0, rnum/0, size/0, 
	 stat/0, copy/1, restore/2, setmst/2]).

%% Table API exports
-export([update/2, setindex/2, genuid/0, query_limit/2, query_limit/3,
	 query_add_condition/4, query_order/3, search/1, searchcount/1,
	 searchout/1]).


%% @spec start() -> {ok, Pid} | Error:term()
%%
%% @doc Start the medici application.
start() ->
    application:start(medici).

%% @spec start(StartupOptions::proplist()) -> {ok, Pid} | Error:term()
%%
%% @doc 
%% Start the medici application, using a provided proplist as a set of
%% additional options for the medici application.  WARNING: If you use
%% start/1 the options you provide will become the new default startup
%% options until you restart your Erlang VM.
%% @end
start(StartupOptions) when is_list(StartupOptions) ->
    {ok, AppEnvOptions} = application:get_env(medici, options),
    CombinedOptions = [StartupOptions | AppEnvOptions],
    MediciOptions = [{K, proplists:get_value(K, CombinedOptions)} || 
			K <- proplists:get_keys(CombinedOptions)],
    application:put_env(medici, {options, MediciOptions}),
    application:start(medici).

stop() ->
    application:stop(medici).

put(Key, Value) ->
    gen_server:call(medici, {put, Key, Value}).
 
putcat(Key, Value) ->
    gen_server:call(medici, {putcat, Key, Value}).

putkeep(Key, Value) ->
    gen_server:call(medici, {putkeep, Key, Value}).

putshl(Key, Value, Width) ->
    gen_server:call(medici, {putshl, Key, Value, Width}).

putnr(Key, Value) ->
    gen_server:cast(medici, {putnr, Key, Value}).

out(Key) ->
    gen_server:call(medici, {out, Key}).

get(Key) ->
    gen_server:call(medici, {get, Key}).

mget(KeyList) ->
    gen_server:call(medici, {mget, KeyList}).

vsiz(Key) ->
    gen_server:call(medici, {vsiz, Key}).

iterinit() ->
    gen_server:call(medici, {iterinit}).

iternext() ->
    gen_server:call(medici, {iternext}).

fwmkeys(Prefix, MaxKeys) ->
    gen_server:call(medici, {fwmkeys, Prefix, MaxKeys}).

addint(Key, Int) ->
    gen_server:call(medici, {addint, Key, Int}).

adddouble(Key, Double) ->
    gen_server:call(medici, {adddouble, Key, Double}).

adddouble(Key, IntPart, FracPart) ->
    gen_server:call(medici, {adddouble, Key, IntPart, FracPart}).

sync() ->
    gen_server:call(medici, {sync}).

vanish() ->
    gen_server:call(medici, {vanish}).

rnum() ->
    gen_server:call(medici, {rnum}).

size() ->
    gen_server:call(medici, {size}).

stat() ->
    gen_server:call(medici, {stat}).

copy(PathName) ->
    gen_server:call(medici, {copy, PathName}).

restore(PathName, TimeStamp) ->
    gen_server:call(medici, {restore, PathName, TimeStamp}).

setmst(HostName, Port) ->
    gen_server:call(medici, {setmst, HostName, Port}).

%% Additional table functions
update(Key, NewCols) ->
    gen_server:call(medici, {update, Key, NewCols}).

setindex(Column, Type) ->
    gen_server:call(medici, {setindex, Column, Type}).

genuid() ->
    gen_server:call(medici, {genuid}).

query_limit(OldQuery, Max) ->
    gen_server:call(medici, {query_limit, OldQuery, Max}).

query_limit(OldQuery, Max, Skip) ->
    gen_server:call(medici, {query_limit, OldQuery, Max, Skip}).

query_add_condition(OldQuery, Column, Op, ExprList) ->
    gen_server:call(medici, {query_add_condition, OldQuery, Column, Op, ExprList}).

query_order(OldQuery, Column, Type) ->
    gen_server:call(medici, {query_order, OldQuery, Column, Type}).

search(Query) ->
    gen_server:call(medici, {search, Query}).

searchcount(Query) ->
    gen_server:call(medici, {searchcount, Query}).

searchout(Query) ->
    gen_server:call(medici, {searchout, Query}).
