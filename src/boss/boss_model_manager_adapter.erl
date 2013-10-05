-module (boss_model_manager_adapter).
-export ([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info (callbacks) ->
  [ {start, 0}, {stop, 0}, {compile, 2}, {edoc_module, 2},
    {is_model_instance, 2}, {dummy_instance, 1},
    {to_json, 1} %, {from_json, 1}
  ];
behaviour_info (_Other) -> undefined.
