-module(admin_lib).
-compile(export_all).

push_update(updated, {Record, Attr, OldVal, NewVal}, Channel) ->
    boss_mq:push(Channel, [{ev, updated}, {data, [{id, Record:id()}, {attr, Attr}, {val, NewVal}]}]).

uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [D,H,M,S])).
