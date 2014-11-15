-module(boss_log_util_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").

gen_trace_without_location() ->
    [
     {my_module, my_func, 1, []},
     {other_module, other_func, 3, []}
    ].

gen_trace_with_location() ->
    [
     {my_module, my_func, 1, [{file, "life_the_universe_and_everything.erl"}, {line, 42}]},
     {other_module, other_func, 3, [{file, "deep_thought.erl"}, {line, 1337}]}
    ].

default_stacktrace_format_with_location_test() ->
    application:set_env(boss, log_stack_multiline, false),
    ?assertEqual(
       "error:missing_question Stacktrace: [{my_module,my_func,1,[{file,\"life_the_universe_and_everything.erl\"},{line,42}]},{other_module,other_func,3,[{file,\"deep_thought.erl\"},{line,1337}]}]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_with_location()))
      ).

default_stacktrace_format_without_location_test() ->
    application:set_env(boss, log_stack_multiline, false),
    ?assertEqual(
       "error:missing_question Stacktrace: [{my_module,my_func,1,[]},{other_module,other_func,3,[]}]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_without_location()))
      ).

multiline_stacktrace_format_with_location_test() ->
    application:set_env(boss, log_stack_multiline, true),
    ?assertEqual(
       "error:missing_question Stacktrace: \n life_the_universe_and_everything.erl:42 [my_module:my_func/1]\n deep_thought.erl:1337 [other_module:other_func/3]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_with_location()))
      ).

multiline_stacktrace_format_without_location_test() ->
    application:set_env(boss, log_stack_multiline, true),
    ?assertEqual(
       "error:missing_question Stacktrace: \n [my_module:my_func/1]\n [other_module:other_func/3]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_without_location()))
      ).
