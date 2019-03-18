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

-module(types).

-type execution_mode()        :: 'development' | 'production'.
-type application()              :: atom().
-type language()        :: any().
-type webserver()               :: any().
-type cb_node()                 :: node().
-type controller()              :: any().
-type compiler_adapters() :: 'boss_compiler_adapter_elixir' | 'boss_compiler_adapter_erlang' | 'boss_compiler_adapter_lfe'.
-export_type([execution_mode/0, application/0, language/0, webserver/0, cb_node/0]).
-export_type([controller/0, compiler_adapters/0]).
