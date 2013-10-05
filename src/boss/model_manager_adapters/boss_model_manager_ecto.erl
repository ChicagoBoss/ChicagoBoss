-module(boss_model_manager_ecto).
-behaviour(boss_model_manager_adapter).

-export([
    start/0,
    stop/0,
    compile/2,
    edoc_module/2,
    is_model_instance/2,
    dummy_instance/1,
    to_json/1 %, from_json/1
]).

start() ->
    pgsql = boss_env:get_env(db_adapter, pgsql),
    'Elixir.Boss.Repo.Sup':start_link().

stop() ->
    ok.

compile(ModulePath, Options) ->
    Files = [list_to_binary(ModulePath)],
    [CompiledModule|_] = case proplists:get_value(out_dir, Options) of
        undefined ->
            'Elixir.Kernel.ParallelCompiler':files(Files);
        OutDir ->
            'Elixir.Kernel.ParallelCompiler':files_to_path(Files, OutDir)
    end,
    {ok, CompiledModule}.

edoc_module(_ModulePath, _Options) ->
    not_implemented.

is_model_instance(Object, AvailableModels) ->
    lists:member(element(1, Object), AvailableModels).

dummy_instance(Model) ->
    Model:new([]).

to_json(Object) ->
    {struct, Object:'__record__'(fields)}.
