-module(boss_router_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_REVERSE_ROUTES_TABLE, boss_reverse_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).
-record(boss_route, {number, url, pattern, application, controller, action, params = []}).
-record(boss_reverse_route, {application_controller_action_params, url}).
-record(boss_handler, {status_code, application, controller, action, params = []}).

-record(state, {
        application,
        controllers = [],
        routes_table_id		::atom() | ets:tid(),
        reverse_routes_table_id ::atom() | ets:tid(),
        handlers_table_id	::atom() | ets:tid()
    }).

-spec code_change(_,_,_) -> {'ok',_}.
-spec handle_call('get_all' | 'reload' | {'handle',_} | {'route',_} | {'set_controllers',_} | {'unroute',_,_,[tuple()]},_,#state{}) -> {'reply',_,#state{}}.
-spec handle_cast(_,_) -> {'noreply',_}.
-spec handle_info(_,_) -> {'noreply',_}.
-spec init([any()]) -> {'ok',#state{}}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
-spec start_link(_) -> 'ignore' | {'error',_} | {'ok',pid()}.
-spec terminate(_,#state{}) -> 'true'.
start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    ReverseRoutesTableId = ets:new(?BOSS_REVERSE_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    State = #state{ application = BossApp, 
        routes_table_id = RoutesTableId, 
        reverse_routes_table_id = ReverseRoutesTableId,
        handlers_table_id = HandlersTableId, 
        controllers = Controllers },
    load(State),
    {ok, State}.

handle_call(reload, _From, State) ->
    ets:delete_all_objects(State#state.routes_table_id),
    ets:delete_all_objects(State#state.reverse_routes_table_id),
    ets:delete_all_objects(State#state.handlers_table_id),
    load(State),
    {reply, ok, State};
handle_call({handle, StatusCode}, _From, State) ->
    Result = handle(StatusCode, State),
    {reply, Result, State};
handle_call({route, ""}, From, State) ->
    handle_call({route, "/"}, From, State);
handle_call({route, Url}, _From, State) ->
    Route = route(Url, State),
    {reply, Route, State};
handle_call({unroute, Controller, undefined, Params}, From, State) ->
    handle_call({unroute, Controller, default_action(State, Controller), Params}, From, State);
handle_call({unroute, Controller, Action, Params}, _From, State) ->
    RoutedURL = unroute(Controller, Action, Params, State),
    {reply, RoutedURL, State};
handle_call(get_all, _From, State) ->
    Res = get_all(State),
    {reply, Res, State};
handle_call({set_controllers, ControllerList}, _From, State) ->
    {reply, ok, State#state{ controllers = ControllerList }}.

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.routes_table_id),
    ets:delete(State#state.reverse_routes_table_id),
    ets:delete(State#state.handlers_table_id).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% v Private Stuff Below v %%

load(State) ->
    RoutesFile = boss_files:routes_file(State#state.application),
    error_logger:info_msg("Loading routes from ~p ....", [RoutesFile]),
    case file:consult(RoutesFile) of
        {ok, OrderedRoutes} -> 
            lists:foldl(fun
                    ({UrlOrStatusCode, Proplist}, Number) when is_list(Proplist) ->
                        TheApplication = proplists:get_value(application, Proplist, State#state.application),
                        TheController = proplists:get_value(controller, Proplist),
                        TheAction = proplists:get_value(action, Proplist),
						CleanParams = clean_params(Proplist),
                        case UrlOrStatusCode of
                            Url when is_list(Url) ->
                                {ok, MP} = re:compile("^"++Url++"$"),
                                NewRoute = #boss_route{ 
                                    number = Number,
                                    url = Url, 
                                    pattern = MP,
                                    application = TheApplication, 
                                    controller = TheController, 
                                    action = TheAction, 
                                    params = CleanParams },
                                NewReverseRoute = #boss_reverse_route{
                                    application_controller_action_params = {
                                        TheApplication,
                                        TheController,
                                        TheAction,
                                        lists:keysort(1, CleanParams)
                                    },
                                    url = Url
                                },
                                true = ets:insert(State#state.routes_table_id, NewRoute),
                                true = ets:insert(State#state.reverse_routes_table_id, NewReverseRoute);
                            StatusCode when is_integer(StatusCode) ->
                                NewHandler = #boss_handler{ 
                                    status_code = StatusCode, 
                                    application = TheApplication,
                                    controller = TheController,
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(State#state.handlers_table_id, NewHandler)
                        end,
                        Number+1
                end, 1, OrderedRoutes);
        Error -> 
            error_logger:error_msg("Missing or invalid boss.routes file in ~p~n~p~n", [RoutesFile, Error])
    end.

handle(StatusCode, State) ->
    _Result = case ets:lookup(State#state.handlers_table_id, StatusCode) of
        [] ->
            not_found;
        [#boss_handler{ application = App, controller = C, action = A, params = P }] ->
            ControllerModule = list_to_atom(boss_files:web_controller(App, C, State#state.controllers)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end.

route(Url, State) ->
    _Route = case get_match(Url, ets:tab2list(State#state.routes_table_id)) of
        undefined -> 
            case string:tokens(Url, "/") of
                [Controller] -> 
                    case is_controller(State, Controller) of
                        true -> {ok, {State#state.application, Controller, default_action(State, Controller), []}};
                        false -> not_found
                    end;
                [Controller, Action|Tokens] ->
                    case is_controller(State, Controller) of
                        true -> 
                            UnquotedTokens = lists:map(fun mochiweb_util:unquote/1, Tokens),
                            {ok, {State#state.application, Controller, Action, UnquotedTokens}};
                        false -> not_found
                    end;
                _ ->
                    not_found
            end;
        _Rte = #boss_route{ application = App, controller = C, action = A, params = P } -> 
            lager:info("Boss Route ~p ~p ~p ~p", [App, C, A, P]),
            ControllerModule = list_to_atom(boss_files:web_controller(App, C, State#state.controllers)),
            {Tokens, []}     = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end.

unroute(Controller, Action, Params, State) ->
    _RoutedURL = case ets:lookup(State#state.reverse_routes_table_id,
            {State#state.application, Controller, Action, lists:keysort(1, Params)}) of
        [#boss_reverse_route{ url = Url }] -> Url;
        [] -> undefined
    end.

get_all(State) ->
    _Res = lists:map(fun(#boss_route{ url = U, application = App, controller = C, action = A, params = P }) -> 
                [{url, U}, {application, App}, {controller, C}, {action, A}, {params, P}]
        end, lists:flatten(ets:match(State#state.routes_table_id, '$1'))).

clean_params(Params) ->
    lists:foldl(fun(Key, Vars) ->
        proplists:delete(Key, Vars)
    end, Params, [application, controller, action]).

is_controller(State, Controller) -> 
    boss_files:is_controller_present(State#state.application, Controller, State#state.controllers).

default_action(State, Controller) ->
    case is_controller(State, Controller) of
        true ->
            ControllerModule = list_to_atom(boss_files:web_controller(State#state.application, 
                    Controller, State#state.controllers)),
            case proplists:get_value(default_action, ControllerModule:module_info(attributes)) of
                [DefaultAction] when is_atom(DefaultAction) ->
                    atom_to_list(DefaultAction);
                _ ->
                    "index"
            end;
        false ->
            "index"
    end.

substitute_params(Params, Matches) ->
    substitute_params(Params, Matches, []).

substitute_params([], _Matches, FinalParams) ->
    lists:reverse(FinalParams);
substitute_params([{Key, Value}|Rest], Matches, FinalParams) when is_integer(Value) ->
    substitute_params(Rest, Matches, [{Key, lists:nth(Value, Matches)}|FinalParams]);
substitute_params([{Key, Value}|Rest], Matches, FinalParams) ->
    substitute_params(Rest, Matches, [{Key, Value}|FinalParams]).

get_match(_, []) ->
    undefined;
get_match(Url, [Route|T]) ->
    Params = [
        {controller, Route#boss_route.controller},
        {action, Route#boss_route.action} | Route#boss_route.params
    ],
	MP = Route#boss_route.pattern,
    {IndexedParams, Vars} = index_and_extract_params(Params),
    case re:run(Url, MP, [{capture, Vars, list}]) of
        {match, Matches} ->
            UpdatedParams = substitute_params(IndexedParams, Matches),
            NewAction = proplists:get_value(action, UpdatedParams),
            NewController = proplists:get_value(controller, UpdatedParams),
            Route#boss_route{
                action=NewAction,
                controller=NewController,
                params = clean_params(UpdatedParams)
            };
        match ->
            Route;
        _ ->
            get_match(Url, T)
    end.

index_and_extract_params(Params) ->
    {IndexedParams, Vars} = lists:mapfoldl(fun
        ({Key, Value}, Acc) when is_atom(Value) ->
            case atom_to_list(Value) of
                [$$, C | Rest] when C >= $0, C =< $9 ->
                    {{Key, length(Acc)+1}, [list_to_integer([C|Rest])|Acc]};
                "$"++VarName ->
                    {{Key, length(Acc)+1}, [VarName|Acc]};
                _ ->
                    {{Key, Value}, Acc}
            end;
        ({Key, Value}, Acc) ->
            {{Key, Value}, Acc}
    end, [], Params),
    {IndexedParams, lists:reverse(Vars)}.

