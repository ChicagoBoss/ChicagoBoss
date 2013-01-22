%% Author: jgordor
%% Created: 01/04/2011
%% Description: Minimalist Router system for Chicago Boss
-module(boss_router).

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/0]).
-export([reload/1, route/2, unroute/3, unroute/4, handle/2, get_all/1, set_controllers/2]).

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

unroute(Pid, Controller, Action, Params) when is_pid(Pid) ->
    gen_server:call(Pid, {unroute, Controller, Action, Params});
unroute(Module, Controller, Action, Params) ->
    {ok, Application} = application_controller:get_application_module(Module),
    Pid = boss_web:router_pid(Application),
    case Pid of
        undefined ->
            "";
        _ ->
            unroute(Pid, unatomize(Controller), unatomize(Action), Params)
    end.
unroute(Module, Controller, Action) ->
    unroute(Module, Controller, Action, []).

unatomize(Element) ->
    case is_atom(Element) of
        true ->
            atom_to_list(Element);
        false ->
            Element
    end.

handle(Pid, StatusCode) ->
    gen_server:call(Pid, {handle, StatusCode}).

get_all(Pid) ->
    gen_server:call(Pid, get_all).

set_controllers(Pid, Controllers) ->
    gen_server:call(Pid, {set_controllers, Controllers}).
