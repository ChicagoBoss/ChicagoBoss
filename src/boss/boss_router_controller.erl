-module(boss_router_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).
-record(boss_route, {url, pattern, application, controller, action, params = []}).
-record(boss_handler, {status_code, application, controller, action, params = []}).

-record(state, {
        application,
        controllers = [],
        routes_table_id,
        handlers_table_id
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    State = #state{ application = BossApp, routes_table_id = RoutesTableId, 
        handlers_table_id = HandlersTableId, controllers = Controllers },
    load(State),
    {ok, State}.

handle_call(reload, _From, State) ->
    ets:delete_all_objects(State#state.routes_table_id),
    ets:delete_all_objects(State#state.handlers_table_id),
    load(State),
    {reply, ok, State};
handle_call({handle, StatusCode}, _From, State) ->
    Result = case ets:lookup(State#state.handlers_table_id, StatusCode) of
        [] ->
            not_found;
        [#boss_handler{ application = App, controller = C, action = A, params = P }] ->
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end,
    {reply, Result, State};
handle_call({route, ""}, From, State) ->
    handle_call({route, "/"}, From, State);
handle_call({route, Url}, _From, State) ->
    Route = case get_match(Url, ets:tab2list(State#state.routes_table_id)) of
        undefined -> 
            case string:tokens(Url, "/") of
                [Controller] -> 
                    case is_controller(State, Controller) of
                        true -> {ok, {State#state.application, Controller, default_action(State, Controller), []}};
                        false -> not_found
                    end;
                [Controller, Action|Params] ->
                    case is_controller(State, Controller) of
                        true -> {ok, {State#state.application, Controller, Action, Params}};
                        false -> not_found
                    end;
                _ ->
                    not_found
            end;
        #boss_route{ application = App, controller = C, action = A, params = P } -> 
            ControllerModule = list_to_atom(boss_files:web_controller(App, C)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end,
    {reply, Route, State};
handle_call({unroute, Controller, undefined, Params}, From, State) ->
    handle_call({unroute, Controller, default_action(State, Controller), Params}, From, State);
handle_call({unroute, Controller, Action, Params}, _From, State) ->
    RoutedURL = ets:foldl(fun
            (#boss_route{ application = App, controller = C, action = A, params = P } = Route, Default) 
                when App =:= State#state.application, C =:= Controller, A =:= Action ->
                case lists:keysort(1, Params) =:= lists:keysort(1, P) of
                    true ->
                        Route#boss_route.url;
                    false ->
                        Default
                end;
            (_, Default) ->
                Default
        end, undefined, State#state.routes_table_id),
    Result = case RoutedURL of
        undefined ->
            ControllerModule = list_to_atom(boss_files:web_controller(State#state.application, Controller)),
            {Tokens, Variables1} = boss_controller_lib:convert_params_to_tokens(Params, ControllerModule, list_to_atom(Action)),

            URL = case Tokens of
                [] ->
                    lists:concat(["/", Controller, "/", Action]);
                _ ->
                    lists:concat(["/", Controller, "/", Action |
                            lists:foldr(fun(T, Acc) -> ["/", T | Acc] end, [], Tokens)])
            end,
            QueryString = mochiweb_util:urlencode(Variables1),
            case QueryString of
                "" ->
                    URL;
                _ ->
                    URL ++ "?" ++ QueryString
            end;
        _ ->
            RoutedURL
    end,
    {reply, Result, State};
handle_call(get_all, _From, State) ->
    Res = lists:map(fun(#boss_route{ url = U, application = App, controller = C, action = A, params = P }) -> 
                [{url, U}, {application, App}, {controller, C}, {action, A}, {params, P}]
        end, lists:flatten(ets:match(State#state.routes_table_id, '$1'))),
    {reply, Res, State};
handle_call({set_controllers, ControllerList}, _From, State) ->
    {reply, ok, State#state{ controllers = ControllerList }}.

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.routes_table_id),
    ets:delete(State#state.handlers_table_id).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

load(State) ->
    RoutesFile = boss_files:routes_file(State#state.application),
    error_logger:info_msg("Loading routes from ~p ....~n", [RoutesFile]),
    case file:consult(RoutesFile) of
        {ok, Routes} -> 
            lists:map(fun
                    ({UrlOrStatusCode, Proplist}) when is_list(Proplist) ->
                        TheApplication = proplists:get_value(application, Proplist, State#state.application),
                        TheController = proplists:get_value(controller, Proplist),
                        TheAction = proplists:get_value(action, Proplist),
                        CleanParams = lists:foldl(fun(Key, Vars) ->
                                    proplists:delete(Key, Vars)
                            end, Proplist, [application, controller, action]),
                        case UrlOrStatusCode of
                            Url when is_list(Url) ->
                                {ok, MP} = re:compile("^"++Url++"$"),
                                NewRoute = #boss_route{ 
                                    url = Url, 
                                    pattern = MP,
                                    application = TheApplication, 
                                    controller = TheController, 
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(State#state.routes_table_id, NewRoute);
                            StatusCode when is_integer(StatusCode) ->
                                NewHandler = #boss_handler{ 
                                    status_code = StatusCode, 
                                    application = TheApplication,
                                    controller = TheController,
                                    action = TheAction, 
                                    params = CleanParams },
                                true = ets:insert(State#state.handlers_table_id, NewHandler)
                        end
                end, Routes);
        Error -> 
            error_logger:error_msg("Missing or invalid boss.routes file in ~p~n~p~n", [RoutesFile, Error])
    end.

is_controller(State, Controller) -> 
    lists:member(boss_files:web_controller(State#state.application, Controller), State#state.controllers).

default_action(State, Controller) ->
    case is_controller(State, Controller) of
        true ->
            ControllerModule = list_to_atom(boss_files:web_controller(State#state.application, Controller)),
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
get_match(Url, [Route = #boss_route{pattern = MP}|T]) ->
    Params = Route#boss_route.params,
    {IndexedParams, Vars} = lists:mapfoldr(fun
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
    case re:run(Url, MP, [{capture, lists:reverse(Vars), list}]) of
        {match, Matches} ->
            Route#boss_route{ params = substitute_params(IndexedParams, Matches) };
        match ->
            Route;
        _ ->
            get_match(Url, T)
    end.
