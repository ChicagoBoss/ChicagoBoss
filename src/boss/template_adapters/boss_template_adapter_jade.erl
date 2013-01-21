-module(boss_template_adapter_jade).
-compile(export_all).

file_extensions() -> ["jade"].

translatable_strings(Module) -> [].

source(Module) -> "".

dependencies(Module) -> [].

render(Module, Variables, Options) ->
    Module:render(Variables, Options).

compile_file(ViewPath, Module, Options) ->
    OutDir = proplists:get_value(out_dir, Options),
    case jaderl:compile(ViewPath, Module, [{out_dir, OutDir}]) of
        ok -> {ok, Module};
        Err -> Err
    end.
