%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).
-behaviour(boss_router_adapter).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/0]).
-export([find_application_for_path/3]).
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
	error_logger:info_msg("Route: ~pUrl~n~p",[Url, erlang:get_stacktrace()]),
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

find_application_for_path(Req, Path, Applications) ->
    Host    = Req:header(host),
    UseHost = case Host of
        undefined -> undefined;
        _ -> hd(re:split(Host, ":", [{return, list}]))
    end,
    find_application_for_path(UseHost, Path, undefined, Applications, -1).

find_application_for_path(_Host, _Path, Default, [], _MatchScore) ->
    Default;
find_application_for_path(Host, Path, Default, [App|Rest], MatchScore) ->
    DomainScore = case Host of
        undefined -> 0;
        _ ->
            case boss_web:domains(App) of
                all -> 0;
                Domains ->
                    case lists:member(Host, Domains) of
                        true -> 1;
                        false -> -1
                    end
            end
    end,
    BaseURL = boss_web:base_url(App),
    PathScore = length(BaseURL),
    {UseApp, UseScore} = case (DomainScore >= 0) andalso (1000 * DomainScore + PathScore > MatchScore) andalso lists:prefix(BaseURL, Path) of
        true -> {App, DomainScore * 1000 + PathScore};
        false -> {Default, MatchScore}
    end,
    find_application_for_path(Host, Path, UseApp, Rest, UseScore).



