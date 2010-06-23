-module(boss_db_driver).
-export([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info(callbacks) ->
    [
        {start, 0}, {stop, 0}, 
        {find, 1}, {find, 6}, {count, 2}, {delete, 1}, {counter, 1}, {incr, 2}, {save_record, 1}
    ];
behaviour_info(_Other) ->
    undefined.
