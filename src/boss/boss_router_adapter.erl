-module(boss_router_adapter).
-author("chan.sisowath@gmail.com").
-export([behaviour_info/1]).

-spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined.
behaviour_info(callbacks) ->
    [
     {start,                     0} 
    ,{start,                     1}
    ,{stop,                      0}
    ,{find_application_for_path, 3}
    ,{reload,                    1}
    ,{route,                     2} 
    ,{unroute,                   6} 
    ,{handle,                    2} 
    ,{get_all,                   1} 
    ,{set_controllers,           2}
    ];

behaviour_info(_Other) ->
    undefined.
