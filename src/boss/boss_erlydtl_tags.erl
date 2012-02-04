-module(boss_erlydtl_tags).
-export([url/2]).

url(Variables, Options) ->
    ListVars = lists:map(fun 
            ({K, V}) when is_binary(V) -> {K, binary_to_list(V)}; 
            ({K, V}) -> {K, V} 
        end, Variables),
    App = proplists:get_value(application, ListVars, proplists:get_value(application, Options)),
    Controller = proplists:get_value(controller, ListVars, proplists:get_value(controller, Options)),
    DefaultAction = case proplists:get_value(controller, ListVars) of
        undefined ->
            proplists:get_value(action, Options);
        _ ->
            undefined
    end,
    Action = proplists:get_value(action, ListVars, DefaultAction),

    CleanVars = lists:foldl(fun(Key, Vars) ->
                proplists:delete(Key, Vars)
        end, ListVars, [application, controller, action]),

    NoUndefinedVars = lists:foldl(fun
            ({_, undefined}, Acc) ->
                Acc;
            (KV, Acc) ->
                [KV|Acc]
        end, [], CleanVars),

    RouterPid = proplists:get_value(router_pid, Options),
    BaseURL = boss_web:base_url(list_to_atom(lists:concat([App]))),
    BaseURL ++ boss_router:unroute(RouterPid, Controller, Action, NoUndefinedVars).
