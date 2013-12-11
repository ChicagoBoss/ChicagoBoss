-module(boss_elixir_compiler).
-export([compile/2]).

-spec load_code(atom(),binary()) -> 'ok' | {'error',string()}.
-spec compile(maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []),[any()]) -> {'error',[any()]} | {'ok',atom()}.
-spec compile_to_outdir(atom(),binary(),atom() | binary() | [atom() | [any()] | char()]) -> {'error',[any()]} | {'ok',atom()}.
-spec handle_write_result(atom(),binary() | string(), 'ok' | {'error',atom()}) -> {'error',[any()]} | {'ok',atom()}.

load_code(Module, Bin) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".ex", Bin) of
        {module, _} -> ok;
        _           -> {error, lists:concat(["code reload failed: ", Module])}
    end.

compile(FilePath, Options) ->
    [{Module, Binary}] = elixir_compiler:file(list_to_binary(FilePath)),
    load_code(Module, Binary),
    OutDir = proplists:get_value(out_dir, Options),
    compile_to_outdir(Module, Binary, OutDir).


compile_to_outdir(Module, _Binary, undefined) ->
    {ok, Module};
compile_to_outdir(Module, Binary, OutDir) ->
    BeamFile	= filename:join([OutDir, atom_to_list(Module) ++ ".beam"]),
    WriteResult = file:write_file(BeamFile, Binary),
    handle_write_result(Module, BeamFile, WriteResult).


handle_write_result(Module, _BeamFile, ok) ->
    {ok, Module};
handle_write_result(_Module, BeamFile, {error, Reason}) ->
    {error, lists:flatten(
	      io_lib:format("Beam generation of '~s' failed: ~p",
			    [BeamFile, file:format_error(Reason)]))}.
