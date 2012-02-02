-module(boss_db_adapter).
-export([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info(callbacks) ->
    [
        {start, 0}, {start, 1}, {stop, 1}, 
        {find, 2}, {find, 7}, {count, 3}, 
        {delete, 2}, {counter, 2}, {incr, 3}, {save_record, 2}
    ];
behaviour_info(_Other) ->
    undefined.
