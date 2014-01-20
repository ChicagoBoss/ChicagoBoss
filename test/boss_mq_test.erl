-module(boss_mq_test).
-include("std.hrl").


convert_to_ms_test() ->
    ?assertEqual(infinity, boss_mq:convert_to_ms(infinity)),
    ?assertEqual(1000, boss_mq:convert_to_ms(1)).


spec_test_() ->
    gen([
         fun prop_pull_recieve/0,
         fun prop_pull_recieve_timeout/0,
         fun prop_pull_recieve_error/0,
         {stop, 0},
         {convert_to_ms, 1},
         {make_queue_options, 0}
        ], boss_mq).

-type date()     :: {1970..2030, 1..12, 1..31}.
-type time()     :: {0..23, 0..59, 0..60}.
-type datetime() :: {date(), time()}.
prop_pull_recieve() ->
    ?FORALL({Msg, PullTime},
            {{refrence, datetime(), jsx:json_term()},
             datetime()},
             begin
                 self() ! Msg,
                 {ok, NewTimeStamp, Messages}       = boss_mq:pull_recieve(100, {ok, PullTime}),
                 {refrence, NewTimeStamp, Messages} =:= Msg
                     
             end).
prop_pull_recieve_timeout() ->
    ?FORALL({ Pulltime},
             {datetime()},
             begin
                 {ok, Pulltime, []}  =:= boss_mq:pull_recieve(1, {ok, Pulltime})
             end).

prop_pull_recieve_error() ->
    ?FORALL(
       Error,
       {error, string()},

        begin
            Error  =:= boss_mq:pull_recieve(1, Error)
        end).
