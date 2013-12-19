-module(boss_files_util).

-export([root_dir/0]).
-export([root_src_dir/0]).

-type input_string() :: string().


-spec root_dir() -> input_string().
root_dir() -> filename:absname(""). %filename:join([filename:dirname(code:which(?MODULE)), ".."]).


-spec root_src_dir() -> [99 | 114 | 115,...].
root_src_dir() -> "src".
