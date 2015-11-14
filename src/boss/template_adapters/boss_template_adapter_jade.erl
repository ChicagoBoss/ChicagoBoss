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

-module(boss_template_adapter_jade).
-compile(export_all).

file_extensions() -> ["jade"].

translatable_strings(_Module) -> [].

source(_Module) -> "".

dependencies(_Module) -> [].

render(Module, Variables, Options) ->
    Module:render(Variables, Options).

compile_file(ViewPath, Module, Options) ->
    OutDir    = proplists:get_value(out_dir, Options),
    ok        = jaderl:compile(ViewPath, Module, [{out_dir, OutDir}]),
    {ok, Module}.

