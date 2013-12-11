-module(boss_template_adapter_jade).
-compile(export_all).

file_extensions() -> ["jade"].

translatable_strings(_Module) -> [].

source(_Module) -> "".

dependencies(_Module) -> [].

render(Module, Variables, Options) ->
    Module:render(Variables, Options).

compile_file(ViewPath, Module, Options) ->
    OutDir	= proplists:get_value(out_dir, Options),
    ok		= jaderl:compile(ViewPath, Module, [{out_dir, OutDir}]),
    {ok, Module}.

