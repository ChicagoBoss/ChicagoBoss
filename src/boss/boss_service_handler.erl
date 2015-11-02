%%-------------------------------------------------------------------
%% @author 
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright 
%%     This file is part of ChicagoBoss project. 
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc 
%%-------------------------------------------------------------------

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
