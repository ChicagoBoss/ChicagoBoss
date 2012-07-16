-module(boss_mochicow_handler).
-export([init/3, loop/1]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
	boss_web_controller:handle_request(Req, mochiweb_request_bridge, mochiweb_response_bridge).
