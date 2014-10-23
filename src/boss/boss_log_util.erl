-module(boss_log_util).
-export([stacktrace/0]).

stacktrace() ->
    Trace = erlang:get_stacktrace(),
    string:join(
      %% [""|
       lists:map(
         fun(Frame) ->
                 {Module, Func, Arity, [{file, Filename}, {line, LineNo}]} = Frame,
                 io_lib:format(
                   " ~s:~b [~s:~s/~b]",
                   [
                    Filename,
                    LineNo,
                    atom_to_list(Module),
                    atom_to_list(Func),
                    Arity
                   ]
                  )
         end,
         Trace
        )
      %% ]
,
      io_lib:format("~n", [])
     ).
