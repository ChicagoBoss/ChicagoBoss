-module(boss_log_util_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").

gen_trace_without_location(Arg1, Arg2) ->
    [
     {my_module, my_func, Arg1, []},
     {other_module, other_func, Arg2, []}
    ].

gen_trace_with_location(Arg1, Arg2) ->
    [
     {my_module, my_func, Arg1, [{file, "life_the_universe_and_everything.erl"}, {line, 42}]},
     {other_module, other_func, Arg2, [{file, "deep_thought.erl"}, {line, 1337}]}
    ].

default_stacktrace_format_with_location_test() ->
    application:set_env(boss, log_stack_multiline, false),
    ?assertEqual(
       "error:missing_question Stacktrace: [{my_module,my_func,1,[{file,\"life_the_universe_and_everything.erl\"},{line,42}]},{other_module,other_func,3,[{file,\"deep_thought.erl\"},{line,1337}]}]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_with_location(1, 3)))
      ).

default_stacktrace_format_without_location_test() ->
    application:set_env(boss, log_stack_multiline, false),
    ?assertEqual(
       "error:missing_question Stacktrace: [{my_module,my_func,1,[]},{other_module,other_func,3,[]}]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_without_location(1, 3)))
      ).

default_stacktrace_format_with_location_and_args_test() ->
    application:set_env(boss, log_stack_multiline, false),
    ?assertEqual(
       "error:missing_question Stacktrace: [{my_module,my_func,[],[{file,\"life_the_universe_and_everything.erl\"},{line,42}]},{other_module,other_func,[foo],[{file,\"deep_thought.erl\"},{line,1337}]}]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_with_location([], [foo])))
      ).

default_stacktrace_format_without_location_and_args_test() ->
    application:set_env(boss, log_stack_multiline, false),
    ?assertEqual(
       "error:missing_question Stacktrace: [{my_module,my_func,[],[]},{other_module,other_func,[foo],[]}]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_without_location([], [foo])))
      ).

multiline_stacktrace_format_with_location_test() ->
    application:set_env(boss, log_stack_multiline, true),
    ?assertEqual(
       "error:missing_question Stacktrace: \n life_the_universe_and_everything.erl:42 [my_module:my_func/1]\n deep_thought.erl:1337 [other_module:other_func/3]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_with_location(1, 3)))
      ).

multiline_stacktrace_format_without_location_test() ->
    application:set_env(boss, log_stack_multiline, true),
    ?assertEqual(
       "error:missing_question Stacktrace: \n [my_module:my_func/1]\n [other_module:other_func/3]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_without_location(1, 3)))
      ).

multiline_stacktrace_format_with_location_and_args_test() ->
    application:set_env(boss, log_stack_multiline, true),
    ?assertEqual(
       "error:missing_question Stacktrace: \n life_the_universe_and_everything.erl:42 [my_module:my_func()]\n deep_thought.erl:1337 [other_module:other_func(foo, bar)]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_with_location([], [foo, bar])))
      ).

multiline_stacktrace_format_without_location_and_args_test() ->
    application:set_env(boss, log_stack_multiline, true),
    ?assertEqual(
       "error:missing_question Stacktrace: \n [my_module:my_func()]\n [other_module:other_func(foo, \"bar\")]",
       lists:flatten(boss_log_util:format_stacktrace(error, missing_question, gen_trace_without_location([], [foo, "bar"])))
      ).
