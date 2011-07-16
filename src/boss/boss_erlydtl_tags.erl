-module(boss_erlydtl_tags).
-compile(export_all).

url(Variables, Options) ->
    App = proplists:get_value(application, Variables, proplists:get_value(application, Options)),
    Controller = proplists:get_value(controller, Variables, proplists:get_value(controller, Options)),
    DefaultAction = case proplists:get_value(controller, Variables) of
        undefined ->
            proplists:get_value(action, Options);
        _ ->
            undefined
    end,
    Action = proplists:get_value(action, Variables, DefaultAction),

    CleanVars = lists:foldl(fun(Key, Vars) ->
                proplists:delete(Key, Vars)
        end, Variables, [application, controller, action]),

    NoUndefinedVars = lists:foldl(fun
            ({_, undefined}, Acc) ->
                Acc;
            (KV, Acc) ->
                [KV|Acc]
        end, [], CleanVars),

    RouterPid = boss_web:router_pid(list_to_atom(lists:concat([App]))),
    boss_router:unroute(RouterPid, Controller, Action, NoUndefinedVars).
