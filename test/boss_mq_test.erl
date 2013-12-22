-module(boss_mq_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").


convert_to_ms_test() ->
    ?assertEqual(infinity, boss_mq:convert_to_ms(infinity)),
    ?assertEqual(1000, boss_mq:convert_to_ms(1)).

