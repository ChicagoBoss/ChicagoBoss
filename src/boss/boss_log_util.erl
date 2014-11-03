-module(boss_log_util).
-export([stacktrace/0]).

stacktrace() ->
    Multiline = boss_env:get_env(log_stack_multiline, false),
    TraceString = string:join(
                    lists:map(
                      fun(Frame) ->
                              frame(Frame, Multiline)
                      end,
                      erlang:get_stacktrace()
                     ),
                    separator(Multiline)
                   ),
    case Multiline of
        true ->
            [separator(true)|TraceString];
        _ ->
            "[" ++ TraceString ++ "]"
    end.

separator(true) ->
    io_lib:format("~n", []);
separator(_) ->
    ",".

frame({Module, Func, Arity, [{file, Filename}, {line, LineNo}]}, true) ->
    io_lib:format(
      " ~s:~b [~s:~s/~b]",
      [
       Filename,
       LineNo,
       atom_to_list(Module),
       atom_to_list(Func),
       Arity
      ]
     );
frame({Module, Func, Arity, [{file, Filename}, {line, LineNo}]}, _) ->
    io_lib:format(
      "{~s,~s,~b,[{file,\"~s\"},{line,~b}]}",
      [
       atom_to_list(Module),
       atom_to_list(Func),
       Arity,
       Filename,
       LineNo
      ]
     ).
