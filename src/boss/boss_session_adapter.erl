-module(boss_session_adapter).
-export([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info(callbacks) ->
    [
     {start, 0}, 
     {start, 1},
     {stop, 1}, 
     {init, 1},
     {session_exists, 2}, 
     {create_session, 3}, 
     {lookup_session, 2}, 
     {lookup_session_value, 3}, 
     {set_session_value, 4}, 
     {delete_session, 2}, 
     {delete_session_value, 3}
    ];
behaviour_info(_Other) ->
    undefined.
