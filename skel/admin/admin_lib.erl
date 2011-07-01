-module(admin_lib).
-compile(export_all).

push_update(updated, {Record, Attr, OldVal, NewVal}, Channel) ->
    boss_mq:push(Channel, [{ev, updated}, {data, [{id, Record:id()}, {attr, Attr}, {val, NewVal}]}]).
