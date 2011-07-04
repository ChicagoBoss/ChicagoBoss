-module(boss_session_adapter).
-export([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info(callbacks) ->
    [
        {start, 0}, {start, 1}, {stop, 1}, 
        {needs_expiration, 0}, {new_session, 2}, {get_session_data, 2}, 
        {set_session_data, 4}, {delete_session, 2}, {remove_session_data, 3}
    ];
behaviour_info(_Other) ->
    undefined.
