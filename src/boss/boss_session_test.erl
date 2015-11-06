%%-------------------------------------------------------------------
%% @author 
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright 
%%     This file is part of ChicagoBoss project. 
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc 
%%-------------------------------------------------------------------

-module(boss_session_test).
-export([start/0, start/1, stop/0]).

start() ->
    start([]).

start([]) ->
    application:start(boss_session_test).

stop() ->
    application:stop(boss_session_test).
