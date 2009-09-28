-module(boss_db_driver).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{find, 1}, {find, 2}, {delete, 1}, {save_record, 1}];
behaviour_info(_Other) ->
    undefined.
