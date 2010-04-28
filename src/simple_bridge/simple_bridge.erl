% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge).
-export ([
    make_request/2,
    make_response/2
]).

% Return a simple_bridge request module.
make_request(Module, Req) -> simple_bridge_request:make(Module, Req).

% Return a simple_bridge response module.
make_response(Module, Req) -> simple_bridge_response:make(Module, Req).
