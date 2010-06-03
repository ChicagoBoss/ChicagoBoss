% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge_response_wrapper, [Mod, Req, Res]).
-include_lib ("simple_bridge.hrl").
-compile(export_all).

status_code(StatusCode) -> 
    Res1 = Res#response { statuscode=StatusCode },
    ?MODULE:new(Mod, Req, Res1).

header(Name, Value) -> 
    Header = #header { name=Name, value=Value },
    Headers = Res#response.headers,
    Headers1 = [X || X <- Headers, X#header.name /= Name],
    Headers2 = [Header|Headers1],
    Res1 = Res#response { headers=Headers2 },
    ?MODULE:new(Mod, Req, Res1).	

clear_headers() -> 
    Res1 = Res#response { headers=[] },
    ?MODULE:new(Mod, Req, Res1).

cookie(Name, Value) ->
    cookie(Name, Value, "/", 20).

cookie(Name, Value, Path, MinutesToLive) -> 
    Cookie = #cookie { name=Name, value=Value, path=Path, minutes_to_live=MinutesToLive },
    Cookies = Res#response.cookies,
    Cookies1 = [X || X <- Cookies, X#cookie.name /= Name],
    Cookies2 = [Cookie|Cookies1],
    Res1 = Res#response { cookies=Cookies2 },
    ?MODULE:new(Mod, Req, Res1).	

clear_cookies() -> 
    Res1 = Res#response { cookies=[] },
    ?MODULE:new(Mod, Req, Res1).

data(Data) -> 
    Res1 = Res#response { data={data, Data} },
    ?MODULE:new(Mod, Req, Res1).

file(Path) ->
    Res1 = Res#response { data={file, Path} },
    ?MODULE:new(Mod, Req, Res1).

build_response() -> Mod:build_response(Req, Res).
