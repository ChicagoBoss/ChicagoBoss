%%% @author mihawk <mihawk@monolite>
%%% @copyright (C) 2012, mihawk
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2012 by mihawk <mihawk@monolite>

-module(boss_service_handler).
-export([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info(callbacks) ->
    [
        {init, 0}, 
        {handle_join, 3},
        {handle_close, 4},
        {handle_incoming, 4},
        {handle_info, 2},
        {terminate, 2}
    ];
behaviour_info(_Other) ->
    undefined.
