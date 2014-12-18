-module(boss_log_util).
-export([stacktrace/2]).

-ifdef(TEST).
-export([format_stacktrace/3]).
-endif.

-spec stacktrace(atom(), any()) -> [char()].

%% @doc Generates a stacktrace and formats it according to the boss_env setting
%% log_stack_multiline.
stacktrace(Class, Error) ->
    format_stacktrace(Class, Error, erlang:get_stacktrace()).

format_stacktrace(Class, Error, Trace) ->
    Multiline = boss_env:get_env(log_stack_multiline, false),
    TraceString = string:join(
                    lists:map(
                      fun(Frame) ->
                              frame(Frame, Multiline)
                      end,
                      Trace
                     ),
                    separator(Multiline)
                   ),
    TraceOutput = case Multiline of
                      true ->
                          [separator(true)|TraceString];
                      _ ->
                          "[" ++ TraceString ++ "]"
                  end,
    io_lib:format("~p:~p Stacktrace: ~s", [Class, Error, TraceOutput]).

separator(true) ->
    io_lib:format("~n", []);
separator(_) ->
    ",".

multiline_func_info(Module, Func, Arity) when is_integer(Arity) ->
    io_lib:format(" [~s:~s/~b]", [atom_to_list(Module), atom_to_list(Func), Arity]);
multiline_func_info(Module, Func, Args) ->
    io_lib:format(" [~s:~s(~s)]", [atom_to_list(Module), atom_to_list(Func), arg_list(Args)]).

default_func_info(Module, Func, ArityOrArgs) ->
    io_lib:format(
      "~s,~s,~p",
      [
       atom_to_list(Module),
       atom_to_list(Func),
       ArityOrArgs
      ]
     ).

arg_list(Args) ->
    string:join(
      lists:map(
        fun(Arg) ->
                io_lib:format("~p", [Arg])
        end,
        Args),
      ", "
     ).

frame({Module, Func, ArityOrArgs, [{file, Filename}, {line, LineNo}]}, true) ->
    io_lib:format(" ~s:~b", [Filename, LineNo]) ++ multiline_func_info(Module, Func, ArityOrArgs);
frame({Module, Func, ArityOrArgs, []}, true) ->
    multiline_func_info(Module, Func, ArityOrArgs);
frame({Module, Func, ArityOrArgs, [{file, Filename}, {line, LineNo}]}, _) ->
    io_lib:format("{~s,[{file,\"~s\"},{line,~b}]}", [default_func_info(Module, Func, ArityOrArgs), Filename, LineNo]);
frame({Module, Func, ArityOrArgs, []}, _) ->
    io_lib:format("{~s,[]}", [default_func_info(Module, Func, ArityOrArgs)]).
