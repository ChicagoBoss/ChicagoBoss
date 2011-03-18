% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge_response).
-include_lib ("simple_bridge.hrl").
-export ([
    make/2,
    behaviour_info/1
]).


make(Module, ResponseData) ->
    try
        make_nocatch(Module, ResponseData)
    catch Type : Error ->
        error_logger:error_msg("Error in simple_bridge_response:make/2 - ~p - ~p~n~p", [Type, Error, erlang:get_stacktrace()]),
        erlang:Type(Error)
    end.

make_nocatch(Mod, ResponseData) ->
    simple_bridge_response_wrapper:new(Mod, ResponseData, #response{}).

behaviour_info(callbacks) -> [
    {build_response, 2} 
];

behaviour_info(_) -> ok.
