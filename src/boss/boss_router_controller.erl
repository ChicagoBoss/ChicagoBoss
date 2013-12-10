-module(boss_router_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BOSS_REWRITES_TABLE, boss_rewrites).
-define(BOSS_ROUTES_TABLE, boss_routes).
-define(BOSS_REVERSE_ROUTES_TABLE, boss_reverse_routes).
-define(BOSS_HANDLERS_TABLE, boss_handlers).
-record(boss_rewrite, {pattern, indexed_split, vars}).
-record(boss_route, {number, url, pattern, application, controller, action, params = []}).
-record(boss_reverse_route, {application_controller_action_params, url}).
-record(boss_handler, {status_code, application, controller, action, params = []}).

-record(state, {
        application,
        controllers = [],
        rewrites_table_id,
        routes_table_id,
        reverse_routes_table_id,
        handlers_table_id
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    BossApp = proplists:get_value(application, Options),
    Controllers = proplists:get_value(controllers, Options, []),
    RewritesTableId = ets:new(?BOSS_REWRITES_TABLE, [ordered_set, public, {keypos, 2}]),
    RoutesTableId = ets:new(?BOSS_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    ReverseRoutesTableId = ets:new(?BOSS_REVERSE_ROUTES_TABLE, [ordered_set, public, {keypos, 2}]),
    HandlersTableId = ets:new(?BOSS_HANDLERS_TABLE, [ordered_set, public, {keypos, 2}]),
    State = #state{ application = BossApp, 
        rewrites_table_id = RewritesTableId, 
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
    Result = case ets:lookup(State#state.handlers_table_id, StatusCode) of
        [] ->
            not_found;
        [#boss_handler{ application = App, controller = C, action = A, params = P }] ->
            ControllerModule = list_to_atom(boss_files:web_controller(App, C, State#state.controllers)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end,
    {reply, Result, State};
handle_call({route, ""}, From, State) ->
    handle_call({route, "/"}, From, State);
handle_call({route, Url}, _From, State) ->
    RewrittenUrl = rewrite(Url, ets:tab2list(State#state.rewrites_table_id)),
    Route = case get_match(RewrittenUrl, ets:tab2list(State#state.routes_table_id)) of
        undefined -> 
            case string:tokens(RewrittenUrl, "/") of
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
        #boss_route{ application = App, controller = C, action = A, params = P } -> 
            ControllerModule = list_to_atom(boss_files:web_controller(App, C, State#state.controllers)),
            {Tokens, []} = boss_controller_lib:convert_params_to_tokens(P, ControllerModule, list_to_atom(A)),
            {ok, {App, C, A, Tokens}}
    end,
    {reply, Route, State};
handle_call({unroute, Controller, undefined, Params}, From, State) ->
    handle_call({unroute, Controller, default_action(State, Controller), Params}, From, State);
handle_call({unroute, Controller, Action, Params}, _From, State) ->
    RoutedURL = case ets:lookup(State#state.reverse_routes_table_id,
            {State#state.application, Controller, Action, lists:keysort(1, Params)}) of
        [#boss_reverse_route{ url = Url }] -> Url;
        [] -> undefined
    end,
    {reply, RoutedURL, State};
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
    ets:delete(State#state.reverse_routes_table_id),
    ets:delete(State#state.handlers_table_id).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

load(State) ->
    RoutesFile = boss_files:routes_file(State#state.application),
    error_logger:info_msg("Loading routes from ~p ....", [RoutesFile]),
    case file:consult(RoutesFile) of
        {ok, OrderedRoutes} -> 
            lists:foldl(fun
                    ({rewrite, UrlPattern, RewritePattern}, Number) ->
                        UrlSplitList = re:split(RewritePattern, "(\\'\\$\\w+\\')", [group, {return, list}]),
                        {IndexedSplit, Vars} =
                                    lists:mapfoldl(fun([Prefix, VarName], Acc) ->
                                                           {{Prefix, length(Acc)+1}, [lists:sublist(VarName, 3, length(VarName)-3) | Acc]};
                                                      ([String], Acc) ->
                                                           {String, Acc}
                                                   end, [], UrlSplitList),
                        {ok, MP} = re:compile("^"++UrlPattern++"$"),
                        NewRewriteRule = #boss_rewrite{pattern = MP,
                                                       indexed_split = IndexedSplit,
                                                       vars = lists:reverse(Vars)},
                        true = ets:insert(State#state.rewrites_table_id, NewRewriteRule),
                        Number;
                    ({UrlOrStatusCode, Proplist}, Number) when is_list(Proplist) ->
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

rewrite(Url, []) ->
    Url;
rewrite(Url, [#boss_rewrite{pattern = MP, indexed_split = IS, vars = Vars } | T]) ->
    case re:run(Url, MP, [{capture, Vars, list}]) of
        {match, Matches} ->
            RewrittenUrl = lists:foldl(fun({Prefix, Index}, Acc) ->
                                               Acc ++ Prefix ++ lists:nth(Index, Matches);
                                          (String, Acc) ->
                                               Acc ++ String
                                       end, [], IS),
            RewrittenUrl;
        match ->
            Url;
        _ ->
            rewrite(Url, T)
    end.
