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
    AppEnvOptions = case application:get_env(medici, options) of
        {ok, Val} -> Val;
        undefined -> []
    end,
    CombinedOptions = StartupOptions ++ AppEnvOptions,
    %% Merge into a single set of options, favoring those passed in
    %% to start/1 over the app env.
    MediciOptions = [{K, proplists:get_value(K, CombinedOptions)} || 
        K <- proplists:get_keys(CombinedOptions)],
    application:load(medici),
    ok = application:set_env(medici, options, MediciOptions),
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

%% TODO: for completeness, set up two internal suites within this
%% test suite, one for hash and another for table.  Test hash (assuming
%% the server is already running), then test hash again by running the 
%% server internal to medici, then test table the same way.  Need to
%% review eunit docs to get this one right...

-ifdef(EUNIT).

get_random_count() ->
    get_random_count(1000).

get_random_count(Max) ->
    crypto:start(),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    round(Max * random:uniform()).

%% setup_table_data() ->
%%     ColData = [{"rec1", [{"name", "alice"}, {"sport", "baseball"}]},
%% 	       {"rec2", [{"name", "bob"}, {"sport", "basketball"}]},
%% 	       {"rec3", [{"name", "carol"}, {"age", "24"}]},
%% 	       {"rec4", [{"name", "trent"}, {"age", "33"}, {"sport", "football"}]},
%% 	       {"rec5", [{"name", "mallet"}, {"sport", "tennis"}, {"fruit", "apple"}]}
%% 	       ],
%%     lists:foreach(fun({Key, ValProplist}) ->
%% 			  ok = ?MODULE:put(Key, ValProplist)
%% 		  end, ColData).

init_test() ->
    ?MODULE:start(),
    ?MODULE:stop(),
    ok.

%% init_with_args_test() ->
%%     ?MODULE:start([{foobar, 32}]),
%%     {ok, Options} = application:get_env(medici, options),
%%     ?assert(lists:member({foobar, 32}, Options)),
%%     ?MODULE:stop().

medici_api_test_() ->
    {setup, 
     fun() -> ?MODULE:start() end,
     fun(_Cleanup) -> ?MODULE:stop() end,
     [?_test(put_get_unit()),
      ?_test(put_get_random_unit()),
      ?_test(putkeep_unit()),
      ?_test(putcat_unit()),
      ?_test(putshl_unit()),
%%      ?_test(putnr_unit()),
      ?_test(out_unit()),
      ?_test(mget_unit()),
      ?_test(vsiz_unit()),
      ?_test(vanish_unit()),
      ?_test(iter_unit()),
      ?_test(fwmkeys_unit()),
%%      ?_test(addint_unit()),
      ?_test(sync_unit()),
      ?_test(rnum_unit()),
      ?_test(size_unit()),
      ?_test(stat_unit()),
      ?_test(optimize_unit())]
    }.

put_get_unit() ->
    ?assert(?MODULE:put("put_get1", "testval") =:= ok),
    ?assert(?MODULE:put(<<"put_get2">>, <<32,145,56,0,14>>) =:= ok),
    ?assert(?MODULE:get(<<"put_get1">>) =:= <<"testval">>),
    ?assert(?MODULE:get("put_get2") =:= <<32, 145, 56, 0, 14>>).

put_get_random_unit() ->
    ElementCount = get_random_count(),
    PutVals = lists:foldl(fun(_Seq, Acc) ->
				  KeySize = random:uniform(1024),
				  Key = crypto:rand_bytes(KeySize),
				  ValSize = random:uniform(65536),
				  Val = crypto:rand_bytes(ValSize),
				  ok = ?MODULE:put(Key, Val),
				  [{Key, Val} | Acc]
			  end, [], lists:seq(1, ElementCount)),
    lists:foreach(fun({K, V}) ->
			  ?assert(?MODULE:get(K) =:= V)
		  end, PutVals).

putkeep_unit() ->
    ok = ?MODULE:put(<<"putkeep1">>, <<"foo">>),
    ?assert(?MODULE:get(<<"putkeep1">>) =:= <<"foo">>),
    ?assertMatch({error, _}, ?MODULE:putkeep(<<"putkeep1">>, <<"bar">>)),
    ?assert(?MODULE:get(<<"putkeep1">>) =:= <<"foo">>), % no effect if key already exists before putkeep
    ok = ?MODULE:putkeep(<<"putkeep2">>, <<"baz">>),
    ?assert(?MODULE:get(<<"putkeep2">>) =:= <<"baz">>). % puts the key if key does not exist already

putcat_unit() ->
    ok = ?MODULE:put(<<"putcat1">>, <<"foo">>),
    % append "bar" to the end
    ok = ?MODULE:putcat(<<"putcat1">>, <<"bar">>),
    ?assert(?MODULE:get(<<"putcat1">>) =:= <<"foobar">>).

putshl_unit() ->
    ok = ?MODULE:put(<<"putshl">>, <<"foo">>),
    % append "bar" to the end and shift to the left to retain the width of "4"
    ok = ?MODULE:putshl(<<"putshl">>, <<"bar">>, 4),
    ?assert(?MODULE:get(<<"putshl">>) =:= <<"obar">>).

%% putnr_unit() ->
%%     ?MODULE:putnr(<<"putnr1">>, <<"no reply">>),
%%     ?assert(?MODULE:get(<<"putnr1">>) =:= <<"no reply">>).

out_unit() ->
    ok = ?MODULE:put(<<"out1">>, <<"to remove">>),
    ?assert(?MODULE:get(<<"out1">>) =:= <<"to remove">>),
    ok = ?MODULE:out(<<"out1">>),
    ?assertMatch({error, _}, ?MODULE:get(<<"out1">>)).

mget_unit() ->
    ok = ?MODULE:put(<<"mget1">>, <<"alice">>),
    ok = ?MODULE:put(<<"mget2">>, <<"bob">>),
    ok = ?MODULE:put(<<"mget3">>, <<"carol">>),
    ok = ?MODULE:put(<<"mget4">>, <<"trent">>),
    ?assert(?MODULE:mget([<<"mget1">>, <<"mget2">>, 
			  <<"mget3">>, <<"mget4">>]) =:= 
	    [{<<"mget1">>, <<"alice">>}, 
	     {<<"mget2">>, <<"bob">>}, 
	     {<<"mget3">>, <<"carol">>}, 
	     {<<"mget4">>, <<"trent">>}]).

vsiz_unit() ->
    ok = ?MODULE:put(<<"vsiz1">>, <<"vsiz test">>),
    ?assert(?MODULE:vsiz(<<"vsiz1">>) =:= 9).

vanish_unit() ->
    ok = ?MODULE:put(<<"vanish1">>, <<"going away">>),
    ok = ?MODULE:vanish(),
    ?assertMatch({error, _}, ?MODULE:get(<<"vanish1">>)).

iter_unit() ->
    ok = ?MODULE:vanish(),
    ok = ?MODULE:put(<<"a">>, <<"first">>),
    ok = ?MODULE:iterinit(),
    <<"a">> = ?MODULE:iternext(), % "a" should be the first key
    % Now to test a bit of real iteration
    ok = ?MODULE:put(<<"b">>, <<"second">>),
    ok = ?MODULE:put(<<"c">>, <<"third">>),
    ok = ?MODULE:iterinit(),
    One = ?MODULE:iternext(),
    Two = ?MODULE:iternext(),
    Three = ?MODULE:iternext(),
    ?assertMatch({error, _}, ?MODULE:iternext()),
    ?assertMatch([<<"a">>, <<"b">>, <<"c">>], lists:sort([One, Two, Three])).

fwmkeys_unit() ->
    ok = ?MODULE:vanish(),
    ok = ?MODULE:put(<<"fwmkeys1">>, <<"1">>),
    ok = ?MODULE:put(<<"fwmkeys2">>, <<"2">>),
    ok = ?MODULE:put(<<"fwmkeys3">>, <<"3">>),
    ok = ?MODULE:put(<<"fwmkeys4">>, <<"4">>),
    Keys1 = ?MODULE:fwmkeys(<<"fwmkeys">>, 4),
    ?assert(length(Keys1) =:= 4),
    ?assert(lists:member(<<"fwmkeys1">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys2">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys3">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys4">>, Keys1)),
    Keys2 = ?MODULE:fwmkeys(<<"fwmkeys">>, 2),
    ?assert(length(Keys2) =:= 2).

%% addint_unit() ->
%%     ok = ?MODULE:put(<<"addint1">>, 100),
%%     ?assert(?MODULE:addint(<<"addint1">>, 20) =:= 120).

sync_unit() ->
    ?assert(?MODULE:sync() =:= ok).

rnum_unit() ->
    ok = ?MODULE:vanish(),
    ok = ?MODULE:put(<<"rnum1">>, <<"foo">>),
    ok = ?MODULE:put(<<"rnum2">>, <<"foo">>),
    ?assert(?MODULE:rnum() =:= 2),
    ok = ?MODULE:vanish(),
    ?assert(?MODULE:rnum() =:= 0).

size_unit() ->
    OldSize = ?MODULE:size(),
    ok = ?MODULE:put(<<"size1">>, <<"foo">>),
    NewSize = ?MODULE:size(),
    ?assert(NewSize > OldSize).

stat_unit() ->
    StatInfo = ?MODULE:stat(),
    Protocol = proplists:get_value(protver, StatInfo),
    ?assert(list_to_float(Protocol) > 0.9).

optimize_unit() ->
    ?assert(?MODULE:optimize("#bnum=1000000#opts=ld") =:= ok).

-endif.
