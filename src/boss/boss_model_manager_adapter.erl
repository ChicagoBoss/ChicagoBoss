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

-module (boss_model_manager_adapter).

-callback start() -> any().
-callback stop() -> any().
-callback compile(_, _) -> any().
-callback edoc_module(_, _) -> any().
-callback is_model_instance(_, _) -> any().
-callback dummy_instance(_) -> any().
-callback to_json(_) -> any().
