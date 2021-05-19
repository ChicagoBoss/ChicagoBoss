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

-module(boss_compiler_adapter_elixir).
-compile(export_all).

-define(SEPARATOR, ".").

file_extensions() -> ["ex"].

controller_module(AppName, Controller) ->
    lists:concat(["Elixir", ?SEPARATOR, inflector:camelize(atom_to_list(AppName)), ?SEPARATOR,
            inflector:camelize(Controller), "Controller"]).

module_name_for_file(AppName, File) ->
    "Elixir" ++ ?SEPARATOR ++ inflector:camelize(atom_to_list(AppName)) ++
    ?SEPARATOR ++ inflector:camelize(filename:basename(File, ".ex")).

compile_controller(File, Options) ->
    boss_elixir_compiler:compile(File, Options).

compile(File, Options) ->
    boss_elixir_compiler:compile(File, Options).
