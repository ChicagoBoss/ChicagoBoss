%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([parse_path/1]).

%%
%% API Functions
%%

parse_path("/") ->
    {Controller, Action} = boss_web_controller:get_env(front_page, {"hello", "world"}), 
    {ok, {Controller, Action, []}};
parse_path("/" ++ Url) ->
    Tokens = string:tokens(Url, "/"),
    case length(Tokens) of
        1 -> 
			Controller = hd(Tokens),
			Default = {ok, {Controller, default_action(Controller), []}},
			case is_controller(Controller) of
				true -> Default;
				false -> get_route_for("/" ++ Url, Default)
			end;
        N when N >= 2 ->
			{Controller, Action, Params} = {lists:nth(1, Tokens), 
											lists:nth(2, Tokens), 
											lists:nthtail(2, Tokens)},
			Default = {ok, {Controller, Action, Params}},
			case is_controller(Controller) of
				true -> Default;
				false -> get_route_for("/" ++ Url, Default)
			end;
        _ ->
            {not_found, "File not found"}
    end;
parse_path(_) ->
    {not_found, "File not found"}.


%%
%% Local Functions
%%

%% We know that controller (first token) is not really a controller
get_route_for(Url, Default) ->
	Routes = boss_web_controller:get_env(routes, []),
	%% First search named routes
	case proplists:get_value(Url, Routes) of
		undefined ->
			%% No named route found, check for special unknown route or return Default 
			case proplists:get_value(unknown, Routes) of
				undefined -> Default;
				DefaultRoute ->	{ok, DefaultRoute}
			end;
		NamedRoute -> {ok, NamedRoute}
	end.

is_controller(Controller) -> 
	case code:is_loaded(list_to_atom(Controller ++ "_controller")) of
		{file, _Loaded} -> true;
		false -> false
	end.

default_action(Controller) ->
	DefaultAction = boss_web_controller:get_env(default_action, "index"),
	AllDefaultActions = boss_web_controller:get_env(default_actions, [{"admin", "model"}]),
	proplists:get_value(Controller, AllDefaultActions, DefaultAction).	