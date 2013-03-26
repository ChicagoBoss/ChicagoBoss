%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/0]).
-export([reload/1, route/2, unroute/6, handle/2, get_all/1, set_controllers/2]).

%%
%% API Functions
%%

start() ->
    start([]).

start(Options) ->
    boss_router_sup:start_link(Options).

stop() ->
    ok.

reload(Pid) ->	
    gen_server:call(Pid, reload).

route(Pid, Url) ->
    gen_server:call(Pid, {route, Url}).

unroute(Pid, Application, ControllerList, Controller, undefined, Params) ->
    ControllerModule = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
    Action =  case proplists:get_value(default_action, ControllerModule:module_info(attributes)) of
                  [DefaultAction] when is_atom(DefaultAction) ->
                      atom_to_list(DefaultAction);
                  _ ->
                      "index"
              end,
    unroute(Pid, Application, ControllerList, Controller, Action, Params);
unroute(Pid, Application, ControllerList, Controller, Action, Params) ->
    case gen_server:call(Pid, {unroute, Controller, Action, Params}) of
        undefined ->
            % Do the fancy routing in the calling process
            % to take some pressure off the router process.
            % In the future the router needs workers.
            ControllerModule = list_to_atom(boss_files:web_controller(Application, Controller, ControllerList)),
            {Tokens, Variables1} = boss_controller_lib:convert_params_to_tokens(Params, ControllerModule, list_to_atom(Action)),

            URL = case Tokens of
                [] ->
                    lists:concat(["/", Controller, "/", Action]);
                _ ->
                    lists:concat(["/", Controller, "/", Action |
                            lists:foldr(fun(T, Acc) -> ["/", mochiweb_util:quote_plus(T) | Acc] end, [], Tokens)])
            end,
            QueryString = mochiweb_util:urlencode(Variables1),
            case QueryString of
                "" ->
                    URL;
                _ ->
                    URL ++ "?" ++ QueryString
            end;
        RoutedURL ->
            RoutedURL
    end.

handle(Pid, StatusCode) ->
    gen_server:call(Pid, {handle, StatusCode}).

get_all(Pid) ->
    gen_server:call(Pid, get_all).

set_controllers(Pid, Controllers) ->
    gen_server:call(Pid, {set_controllers, Controllers}).
