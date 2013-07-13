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
