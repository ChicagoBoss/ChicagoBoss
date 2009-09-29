-module(boss_db_driver).
-export([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info(callbacks) ->
    [
        {find, 1}, {find, 3}, {find, 4}, {find, 5}, {find, 6},
        {delete, 1}, {counter, 1}, {incr, 1}, {incr, 2},
        {save_record, 1}
    ];
behaviour_info(_Other) ->
    undefined.
