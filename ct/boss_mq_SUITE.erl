%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2013, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 16 Dec 2013 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(boss_mq_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{minutes,10}}].


init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 

    [test_poll].


my_test_case() -> 
    [].


my_test_case(_Config) -> 
    ok.
test_poll() ->
    [].
test_poll(_Config) ->
    boss_mq:push("test", "Test Message"),
    {ok, _T, "Test Message"} = boss_mq:poll("test", last),
    ok.
