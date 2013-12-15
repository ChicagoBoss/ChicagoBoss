-module(boss_web_controller_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").
-export([action/2, wants_session/3]).

action(exit,_) ->
    exit(test);
action(return, _) ->
    ok.

wants_session(_Application, R, _Modules) ->
    R.


call_controller_action_test() ->
    R = boss_web_controller:call_controller_action(?MODULE, exit,[]),
    ?assertEqual( {output, "Process Error see console.log for details\n"}, R),
    ?assertEqual( ok, boss_web_controller:call_controller_action(?MODULE, return,[])).

make_action_session_id_test() ->
    R = make_ref(),
    ?assertEqual(R,  boss_web_controller:make_action_session_id({},{}, {}, R, {})),
    ?assertEqual(undefined, boss_web_controller:make_action_session_id(false,
								       #boss_app_info{}, 
								       {}, 
								       undefined, 
								       ?MODULE)).
