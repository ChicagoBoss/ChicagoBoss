-module(boss_elixir_compiler).
-export([compile/2]).

-type compile_return() :: {ok, module()}|{error, [any()]}.

-spec load_code(module(),binary()) -> 'ok' | {'error',string()}.
-spec compile(string(),[any()]) -> 
		     compile_return().
-spec compile_to_outdir(module(),
			binary(),
			undefined|string()) ->
			       compile_return().
-spec handle_write_result(module(),binary() | string(), 'ok' | {'error',atom()}) -> 
				 compile_return().

load_code(Module, Bin) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".ex", Bin) of
        {module, _} -> ok;
        _           -> {error, lists:concat(["code reload failed: ", Module])}
    end.

compile(FilePath, Options) ->
    [{Module, Binary}]	= elixir_compiler:file(list_to_binary(FilePath)),
    load_code(Module, Binary),
    OutDir		= proplists:get_value(out_dir, Options),
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
