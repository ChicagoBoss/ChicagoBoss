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

-module(boss_router_adapter).

-callback start() -> any().
-callback start(_) -> any().
-callback stop() -> any().
-callback find_application_for_path(_, _, _) -> any().
-callback reload(_) -> any().
-callback route(_, _) -> any().
-callback unroute(_, _, _, _, _, _) -> any().
-callback handle(_, _) -> any().
-callback get_all(_) -> any().
-callback set_controllers(_, _) -> any().
