-module(boss_flash).
-export([get_and_clear/1, add/4, add/3]).

get_and_clear(Req) ->
	case boss_session:get_session_data(Req, boss_flash) of
		undefined -> [];
		BossFlash ->
			boss_session:remove_session_data(Req, boss_flash),
			[{boss_flash, lists:reverse(BossFlash)}]
	end.

add(Req, Type, Title) ->
	add(Req, Type, Title, undefined).
	
add(Req, Type, Title, Message) ->
	Msg = [{method, atom_to_list(Type)}, {title, Title}, {message, Message}],
    Flash = case boss_session:get_session_data(Req, boss_flash) of
		undefined ->
		    [Msg];
		ExistingFlash ->
		    [Msg|ExistingFlash]
	    end,
    boss_session:set_session_data(Req, boss_flash, Flash).
