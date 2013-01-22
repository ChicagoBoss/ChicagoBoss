-module(boss_elixir_compiler).
-export([compile/2]).

load_code(Module, Bin) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".ex", Bin) of
        {module, _} -> ok;
        _ -> {error, lists:concat(["code reload failed: ", Module])}
    end.

compile(FilePath, Options) ->
    [{Module, Binary}] = elixir_compiler:file(list_to_binary(FilePath)),
    load_code(Module, Binary),
    case proplists:get_value(out_dir, Options) of
        undefined -> {ok, Module};
        OutDir ->
            BeamFile = filename:join([OutDir, atom_to_list(Module) ++ ".beam"]),
            case file:write_file(BeamFile, Binary) of
                ok -> {ok, Module};
                {error, Reason} ->
                    {error, lists:flatten(
                            io_lib:format("Beam generation of '~s' failed: ~p",
                                [BeamFile, file:format_error(Reason)]))}
            end
    end.
