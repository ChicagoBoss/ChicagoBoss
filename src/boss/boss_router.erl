%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([initialize/0, reload/0, parse_path/1, base_url/0, get_all/0]).

-define(BOSS_ROUTER_TABLE, boss_routes).
-record(boss_routes, {url, controller, action, params}).

%%
%% API Functions
%%

initialize() ->
	init_routes_table(),	
	load().

reload() ->	
	ets:delete_all_objects(?BOSS_ROUTER_TABLE),
	load().

parse_path("/") ->
	{Controller, Action} = case get_named_route(front_page) of
						{ok, no_named_route} -> {"hello", "world"};
						{ok, {ParsedController, ParsedAction, _}} -> {ParsedController, ParsedAction}
					end,
    {ok, {Controller, Action, []}};
parse_path("/" ++ Url) ->
	case get_named_route("/" ++ Url) of
		{ok, no_named_route} ->	get_route(Url);
		{ok, NamedRoute} -> {ok, NamedRoute};
		{error, _Reason} -> {not_found, "File not found"}
	end;
parse_path(_) ->
    {not_found, "File not found"}.

base_url() ->
    case application:get_env(base_url) of
        {ok, Val} -> Val;
        _ -> ""
    end.

get_all() ->
	lists:map(fun(X) -> 
					[{url, X#boss_routes.url}, {controller, X#boss_routes.controller}, 
					 {action, X#boss_routes.action}, {params, X#boss_routes.params}]
			  end, lists:flatten(ets:match(?BOSS_ROUTER_TABLE, '$1'))).

%%
%% Local Functions
%%

load() ->
	RoutesFile = boss_files:routes_file(),
	error_logger:info_msg("Loading routes from ~p ....~n", [RoutesFile]),
	case file:consult(RoutesFile) of
		{ok, Routes} -> 
			lists:map(fun(X) -> load_route(X) end, Routes);
		Error -> 
			error_logger:error_msg("Missing or invalid boss.routes file in ~p~n~p~n", [RoutesFile, Error])
	end.

load_route(Route) ->
	case validate_route(Route) of
		{ok, {Url, Controller, Action, Params}} ->
			case ets:insert(?BOSS_ROUTER_TABLE, #boss_routes{url=Url, controller=Controller, action=Action, params=Params}) of
				true -> ok;
				Error -> error_logger:error_msg("boss_router:load_route -> error inserting route in ets~n~p~n", [Error])
			end;
		{error, Reason} ->
			error_logger:error_msg("boss_router:load_route -> ~p in route ~p~n", [Route, Reason])
	end.

validate_route(Route) ->
	case Route of
		{front_page, {ParsedController, ParsedAction}} ->
			{ok, {front_page, ParsedController, ParsedAction, undefined}};
		{default_action, ParsedAction} ->
			{ok, {default_action, undefined, ParsedAction, undefined}};
		{default_actions, ParsedActions} ->
			{ok, {default_actions, undefined, ParsedActions, undefined}};
		{not_found_route, {ParsedController, ParsedAction, ParsedParams}} ->
			{ok, {not_found_route, ParsedController, ParsedAction, ParsedParams}};
		{NamedUrl, {ParsedController, ParsedAction, ParsedParams}} ->
			{ok, {NamedUrl, ParsedController, ParsedAction, ParsedParams}};
		_InvalidRoute ->
			{error, "Invalid route, don't match any valid definition"}
	end.

get_route(Url) ->
	Tokens = string:tokens(Url, "/"),
    case length(Tokens) of
        1 -> 
			Controller = hd(Tokens),
			Default = {ok, {Controller, default_action(Controller), []}},
			case is_controller(Controller) of
				true -> Default;
				false -> get_not_controller_route(Default)
			end;
        N when N >= 2 ->
			{Controller, Action, Params} = {lists:nth(1, Tokens), 
											lists:nth(2, Tokens), 
											lists:nthtail(2, Tokens)},
			Default = {ok, {Controller, Action, Params}},
			case is_controller(Controller) of
				true -> Default;
				false -> get_not_controller_route(Default)
			end;
        _ ->
            {not_found, "File not found"}
    end.

get_named_route(Url) ->
    case ets:lookup(?BOSS_ROUTER_TABLE, Url) of
	       [NamedRoute] -> 
			   {ok, {NamedRoute#boss_routes.controller, NamedRoute#boss_routes.action, NamedRoute#boss_routes.params}};
	       [] ->
			   {ok, no_named_route}
	end.

get_not_controller_route(Default) ->
	%% if not_found_route is defined use it, if not, use defaults
	case get_named_route(not_found_route) of
		{ok, no_named_route} -> Default;
		{ok, NamedRoute} -> {ok, NamedRoute}
	end.

is_controller(Controller) -> 
	lists:member(Controller ++ "_controller", boss_files:web_controller_list()).

default_action(Controller) ->
	DefaultAction = case get_named_route(default_action) of
						{ok, no_named_route} -> "index";
						{ok, {_, Action, _}} -> Action
					end,
	AllDefaultActions = case get_named_route(default_actions) of
						{ok, no_named_route} -> [{"admin", "model"}];
						{ok, {_, Actions, _}} -> Actions
					end,
	proplists:get_value(Controller, AllDefaultActions, DefaultAction).

init_routes_table() ->
	ets:new(?BOSS_ROUTER_TABLE, [ordered_set, public, named_table, {keypos, 2}]).