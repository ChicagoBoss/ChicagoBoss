%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc Callbacks for the rr application.
%%-------------------------------------------------------------------

-module(boss_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for boss.
start(_Type, _StartArgs) ->
    boss_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for boss.
stop(_State) ->
    ok.
