%% @doc Chicago Boss registry handler

-module(boss_registry).

-behaviour(gen_server).

-export([start_link/0, start_link/1, get/1, put/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec get(term()) -> term() | undefined.
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            undefined;
        [{_Key, Value}] ->
            Value
    end.

-spec put(term(), term()) -> ok.
put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

init(_Args) ->
    ets:new(?MODULE, [named_table, {read_concurrency, true}]),
    {ok, []}.

handle_call({put, Key, Value}, _From,  State) ->
    ets:insert(?MODULE, {Key, Value}),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

registry_test_() ->
    {foreach,
        fun() ->
            start_link()
        end,
        fun(_) ->
            ok
        end,
        [
            {"registry test",
                fun() ->
                    ?assertEqual(undefined, boss_registry:get(testkey)),
                    ?assertEqual(ok, boss_registry:put(testkey, "testval")),
                    ?assertEqual("testval", boss_registry:get(testkey)),
                    ?assertEqual(ok, boss_registry:put(testkey, "testval2")),
                    ?assertEqual("testval2", boss_registry:get(testkey))
                end
            }
        ]
    }.

-endif.