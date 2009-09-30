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
%%% update these functions or just run gen_server:call() directly.
%%% @end

-module(medici).

%% Starting and stopping the app
-export([start/0, start/1, stop/0]).

%% Basic API exports
-export([put/2, putkeep/2, putcat/2, putshl/3, putnr/2, out/1, get/1, 
	 mget/1, vsiz/1, iterinit/0, iternext/0, fwmkeys/2, addint/2,
	 adddouble/2, adddouble/3, sync/0, vanish/0, rnum/0, size/0, 
	 stat/0, copy/1, restore/2, setmst/2, optimize/1]).

%% Table API exports
-export([update/2, setindex/2, genuid/0, query_limit/2, query_limit/3,
	 query_add_condition/4, query_order/3, search/1, searchcount/1,
	 searchout/1]).

-include("medici.hrl").

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

%% NOTE TO THOSE WHO CHANGE THE CONTROLLER NAME:
%%
%% The following api calls are just simple wrappers around calls
%% to gen_server.  If you change the controller name in the app
%% configuration options you will no longer be able to use this
%% api, but can quite easily acheive the same effect by just using
%% gen_server:call() directly with your controller name where this
%% api uses the ?CONTROLLER_NAME macro.

put(Key, Value) ->
    gen_server:call(?CONTROLLER_NAME, {put, Key, Value}).
 
putcat(Key, Value) ->
    gen_server:call(?CONTROLLER_NAME, {putcat, Key, Value}).

putkeep(Key, Value) ->
    gen_server:call(?CONTROLLER_NAME, {putkeep, Key, Value}).

putshl(Key, Value, Width) ->
    gen_server:call(?CONTROLLER_NAME, {putshl, Key, Value, Width}).

putnr(Key, Value) ->
    gen_server:cast(?CONTROLLER_NAME, {putnr, Key, Value}).

out(Key) ->
    gen_server:call(?CONTROLLER_NAME, {out, Key}).

get(Key) ->
    gen_server:call(?CONTROLLER_NAME, {get, Key}).

mget(KeyList) ->
    gen_server:call(?CONTROLLER_NAME, {mget, KeyList}).

vsiz(Key) ->
    gen_server:call(?CONTROLLER_NAME, {vsiz, Key}).

iterinit() ->
    gen_server:call(?CONTROLLER_NAME, {iterinit}).

iternext() ->
    gen_server:call(?CONTROLLER_NAME, {iternext}).

fwmkeys(Prefix, MaxKeys) ->
    gen_server:call(?CONTROLLER_NAME, {fwmkeys, Prefix, MaxKeys}).

addint(Key, Int) ->
    gen_server:call(?CONTROLLER_NAME, {addint, Key, Int}).

adddouble(Key, Double) ->
    gen_server:call(?CONTROLLER_NAME, {adddouble, Key, Double}).

adddouble(Key, IntPart, FracPart) ->
    gen_server:call(?CONTROLLER_NAME, {adddouble, Key, IntPart, FracPart}).

sync() ->
    gen_server:call(?CONTROLLER_NAME, {sync}).

vanish() ->
    gen_server:call(?CONTROLLER_NAME, {vanish}).

optimize(TuningOptions) ->
    gen_server:call(?CONTROLLER_NAME, {optimize, TuningOptions}).

rnum() ->
    gen_server:call(?CONTROLLER_NAME, {rnum}).

size() ->
    gen_server:call(?CONTROLLER_NAME, {size}).

stat() ->
    gen_server:call(?CONTROLLER_NAME, {stat}).

copy(PathName) ->
    gen_server:call(?CONTROLLER_NAME, {copy, PathName}).

restore(PathName, TimeStamp) ->
    gen_server:call(?CONTROLLER_NAME, {restore, PathName, TimeStamp}).

setmst(HostName, Port) ->
    gen_server:call(?CONTROLLER_NAME, {setmst, HostName, Port}).

%% Additional table functions
update(Key, NewCols) ->
    gen_server:call(?CONTROLLER_NAME, {update, Key, NewCols}).

setindex(Column, Type) ->
    gen_server:call(?CONTROLLER_NAME, {setindex, Column, Type}).

genuid() ->
    gen_server:call(?CONTROLLER_NAME, {genuid}).

query_limit(OldQuery, Max) ->
    gen_server:call(?CONTROLLER_NAME, {query_limit, OldQuery, Max}).

query_limit(OldQuery, Max, Skip) ->
    gen_server:call(?CONTROLLER_NAME, {query_limit, OldQuery, Max, Skip}).

query_add_condition(OldQuery, Column, Op, ExprList) ->
    gen_server:call(?CONTROLLER_NAME, {query_add_condition, OldQuery, Column, Op, ExprList}).

query_order(OldQuery, Column, Type) ->
    gen_server:call(?CONTROLLER_NAME, {query_order, OldQuery, Column, Type}).

search(Query) ->
    gen_server:call(?CONTROLLER_NAME, {search, Query}).

searchcount(Query) ->
    gen_server:call(?CONTROLLER_NAME, {searchcount, Query}).

searchout(Query) ->
    gen_server:call(?CONTROLLER_NAME, {searchout, Query}).

%% EUnit tests
%%
-ifdef(EUNIT).
init_test() ->
    ?MODULE:start(),
    ?MODULE:stop(),
    ok.
-endif.
