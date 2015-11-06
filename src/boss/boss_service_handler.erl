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

-module(boss_service_handler).

-callback init() -> any().
-callback handle_join(_, _, _) -> any().
-callback handle_close(_, _, _, _) -> any().
-callback handle_incoming(_, _, _, _) -> any().
-callback handle_info(_, _) -> any().
-callback terminate(_, _) -> any().
