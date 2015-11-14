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

-module(boss_session_adapter).

-callback start() -> any().
-callback start(_) -> any().
-callback stop(_) -> any().
-callback init(_) -> any().
-callback session_exists(_, _) -> any().
-callback create_session(_, _, _) -> any().
-callback lookup_session(_, _) -> any().
-callback lookup_session_value(_, _, _) -> any().
-callback set_session_value(_, _, _, _) -> any().
-callback delete_session(_, _) -> any().
-callback delete_session_value(_, _, _) -> any().
