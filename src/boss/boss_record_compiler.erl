-module(boss_record_compiler).
-author('emmiller@gmail.com').
-define(DATABASE_MODULE, boss_db).

-export([compile/1, compile/2, edoc_module/1, edoc_module/2]).

%% @spec compile( File::string() ) -> ok | {error, Reason}
%% @equiv compile(File, [])
compile(File) ->
    compile(File, []).

%% @spec compile( File::string(), Options ) -> ok | {error, Reason}
%% @doc Compile an Erlang source file as a BossRecord. Options:
%% `compiler_options' - Passed directly to compile:forms/2. Defaults to [verbose, return_errors].
compile(File, Options) ->
    case parse(File) of
        {ok, Forms} ->
            case compile_to_binary(trick_out_forms(Forms), File, Options) of
                {ok, Module, Bin} ->
                    case proplists:get_value(out_dir, Options) of
                        undefined -> ok;
                        OutDir ->
                            BeamFile = filename:join([OutDir, atom_to_list(Module) ++ ".beam"]),
                            file:write_file(BeamFile, Bin)
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @spec edoc_module( File::string() ) -> {Module::atom(), EDoc}
%% @equiv edoc_module(File, [])
edoc_module(File) ->
    edoc_module(File, []).

%% @spec edoc_module( File::string(), Options ) -> {Module::atom(), EDoc}
%% @doc Return an `edoc_module()' for the given Erlang source file when
%% compiled as a BossRecord.
edoc_module(File, Options) ->
    {ok, Forms} = parse(File),
    edoc_extract:source(trick_out_forms(Forms), edoc:read_comments(File), 
        File, edoc_lib:get_doc_env([]), Options).

parse(File) ->
    case epp:parse_file(File, [filename:dirname(File)], []) of
        {ok, Forms} ->
            case proplists:get_all_values(error, Forms) of
                [] ->
                    {ok, Forms};
                Errors ->
                    {error, {File, Errors}}
            end;
        Error ->
            Error
    end.

compile_to_binary(Forms, File, Options) ->
    case compile:forms(erl_syntax:revert_forms(Forms), 
            proplists:get_value(compiler_options, Options, [verbose, return_errors])) of
        {ok, Module1, Bin} ->
            code:purge(Module1),
            case code:load_binary(Module1, File, Bin) of
                {module, _} -> {ok, Module1, Bin};
                _ -> {error, lists:concat(["code reload failed: ", Module1])}
            end;
        OtherError ->
            OtherError
    end.

trick_out_forms([{attribute, _, file, {_FileName, _FileNum}}|Rest]) ->
    trick_out_forms(Rest);

trick_out_forms([
        {attribute, _, module, {ModuleName, Parameters}}
        | _T] = Forms) ->
    trick_out_forms(Forms, ModuleName, Parameters).

trick_out_forms(Forms, ModuleName, Parameters) ->
    Attributes = proplists:get_value(attributes, erl_syntax_lib:analyze_forms(Forms)),
    [{eof, _Line}|OtherForms] = lists:reverse(Forms),
    Counters = lists:foldl(
        fun
            ({counter, Counter}, Acc) -> [Counter|Acc];
            (_, Acc) -> Acc
        end, [], Attributes),

    override_functions(
        lists:reverse(OtherForms) ++ 
        save_forms(ModuleName) ++
        validate_forms(ModuleName) ++
        set_attributes_forms(ModuleName, Parameters) ++
        get_attributes_forms(ModuleName, Parameters) ++
        attribute_names_forms(ModuleName, Parameters) ++
        counter_getter_forms(Counters) ++
        counter_reset_forms(Counters) ++
        counter_incr_forms(Counters) ++
        association_forms(ModuleName, Attributes) ++
        parameter_getter_forms(Parameters) ++
        parameter_setter_forms(ModuleName, Parameters) ++
        []).

override_functions(Forms) ->
    override_functions(Forms, [], []).

override_functions([{'function', _, Name, Arity, _} = Function|Rest], Acc, DefinedFunctions) ->
    case lists:member({Name, Arity}, DefinedFunctions) of
        true -> override_functions(Rest, Acc, DefinedFunctions);
        false -> override_functions(Rest, [Function|Acc], [{Name, Arity}|DefinedFunctions])
    end;
override_functions([{tree, 'function', _, {'function', {tree, 'atom', _, Name}, 
                [{tree, 'clause', _, {'clause', Args, _, _}}|_]
            }} = Function|Rest], 
    Acc, DefinedFunctions) ->
    Arity = length(Args),
    case lists:member({Name, Arity}, DefinedFunctions) of
        true -> override_functions(Rest, Acc, DefinedFunctions);
        false -> override_functions(Rest, [Function|Acc], [{Name, Arity}|DefinedFunctions])
    end;
override_functions([H|T], Acc, DefinedFunctions) ->
    override_functions(T, [H|Acc], DefinedFunctions);
override_functions([], Acc, _) ->
    lists:reverse(Acc).

validate_forms(ModuleName) ->
    [erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec validate() -> ok | {error, [ErrorMessages]}",
                        lists:concat(["% @doc Validates this `", ModuleName, "' without saving to the database."]),
                        "% Errors are generated from this model's `validation_tests/0' function (if defined), ",
                        "% which should return a list of `{TestFunction, ErrorMessage}' tuples. For each test, ",
                        "% `TestFunction' should be a fun of arity 0 that returns `true' if the record is valid ",
                        "% or `false' if it is invalid. `ErrorMessage' should be a (constant) string that will be ",
                        "% included in `ErrorMessages' if the associated `TestFunction' returns `false' on this ",
                        lists:concat(["% particular `", ModuleName, "'."])
                    ])], 
            erl_syntax:function(
                erl_syntax:atom(validate),
                [erl_syntax:clause([], none,
                        [erl_syntax:application(
                                erl_syntax:atom(?DATABASE_MODULE),
                                erl_syntax:atom(validate_record),
                                [erl_syntax:variable("THIS")]
                            )])]))].

save_forms(ModuleName) ->
    [erl_syntax:add_precomments([erl_syntax:comment(
                    [lists:concat(["% @spec save() -> {ok, Saved", inflector:camelize(atom_to_list(ModuleName)), "} | {error, [ErrorMessages]}"]),
                        lists:concat(["% @doc Saves this `", ModuleName, "' record to the database. The returned record"]),
                        "% will have an auto-generated ID if the record's ID was set to 'id'.",
                        "% Performs validation first, returning `ErrorMessages' if validation fails.  See `validate/0'."])],
            erl_syntax:function(
                erl_syntax:atom(save),
                [erl_syntax:clause([], none,
                        [erl_syntax:application(
                                erl_syntax:atom(?DATABASE_MODULE),
                                erl_syntax:atom(save_record),
                                [erl_syntax:variable("THIS")]
                            )])]))].  
parameter_getter_forms(Parameters) ->
    lists:map(fun(P) -> 
                erl_syntax:add_precomments([erl_syntax:comment(
                        [lists:concat(["% @spec ", parameter_to_colname(P), "() -> ", P]),
                            lists:concat(["% @doc Returns the value of `", P, "'"])])],
                    erl_syntax:function(
                        erl_syntax:atom(parameter_to_colname(P)),
                        [erl_syntax:clause([], none, [erl_syntax:variable(P)])]))
        end, Parameters).

parameter_setter_forms(ModuleName, Parameters) ->
    lists:map(
        fun(P) ->
                erl_syntax:add_precomments([erl_syntax:comment(
                        [
                            lists:concat(["% @spec ", parameter_to_colname(P), "( ", P, "::", 
                                    case lists:suffix("Time", atom_to_list(P)) of 
                                        true -> "tuple()";
                                        false -> "string()"
                                    end, " ) -> ", inflector:camelize(atom_to_list(ModuleName))]),
                            lists:concat(["% @doc Set the value of `", P, "'."])])],
                    erl_syntax:function(
                        erl_syntax:atom(parameter_to_colname(P)),
                        [erl_syntax:clause([erl_syntax:variable("NewValue")], none,
                                [
                                    erl_syntax:application(
                                        erl_syntax:atom(ModuleName),
                                        erl_syntax:atom(new),
                                        lists:map(
                                            fun
                                                (Param) when Param =:= P ->
                                                    erl_syntax:variable("NewValue");
                                                (Other) ->
                                                    erl_syntax:variable(Other)
                                            end, Parameters))
                                ])]))
        end, Parameters).

get_attributes_forms(ModuleName, Parameters) ->
    [erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec attributes() -> [{Key::atom(), Value::string() | undefined}]",
                        lists:concat(["% @doc A proplist of the `", ModuleName, "' parameters and their values."])])],
            erl_syntax:function(
                erl_syntax:atom(attributes),
                [erl_syntax:clause([], none,
                        [erl_syntax:list(lists:map(fun(P) ->
                                            erl_syntax:tuple([
                                                    erl_syntax:atom(parameter_to_colname(P)),
                                                    erl_syntax:variable(P)])
                                    end, Parameters))])]))].

set_attributes_forms(ModuleName, Parameters) ->
    [erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec attributes(Proplist) -> "++inflector:camelize(atom_to_list(ModuleName)),
                        "% @doc Set multiple record attributes at once. Does not save the record."])],
            erl_syntax:function(
                erl_syntax:atom(attributes),
                [erl_syntax:clause([erl_syntax:variable("Proplist")], none,
                        [erl_syntax:application(
                                erl_syntax:atom(ModuleName),
                                erl_syntax:atom(new),
                                lists:map(fun(P) ->
                                            erl_syntax:application(
                                                erl_syntax:atom(proplists),
                                                erl_syntax:atom(get_value),
                                                [erl_syntax:atom(parameter_to_colname(P)),
                                                    erl_syntax:variable("Proplist"),
                                                    erl_syntax:variable(P)])
                                    end, Parameters))])]))].

association_forms(ModuleName, Attributes) ->
    {Forms, BelongsToList} = lists:foldl(
        fun
            ({has_many, HasMany}, {Acc, BT}) ->
                {has_many_forms(HasMany, ModuleName) ++ Acc, BT};
            ({has_up_to, {Limit, HasMany}}, {Acc, BT}) ->
                {has_many_forms(HasMany, ModuleName, Limit) ++ Acc, BT};
            ({has_up_to, {Limit, HasMany, Sort}}, {Acc, BT}) ->
                {has_many_forms(HasMany, ModuleName, Limit, Sort) ++ Acc, BT};
            ({has_up_to, {Limit, HasMany, Sort, SortOrder}}, {Acc, BT}) ->
                {has_many_forms(HasMany, ModuleName, Limit, Sort, SortOrder) ++ Acc, BT};
            ({belongs_to, BelongsTo}, {Acc, BT}) ->
                {[belongs_to_forms(BelongsTo, ModuleName)|Acc], [BelongsTo|BT]};
            (_, Acc) ->
                Acc
        end, {[], []}, Attributes),
    Forms ++ belongs_to_list_forms(BelongsToList).


belongs_to_list_forms(BelongsToList) ->
    [ erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec belongs_to_names() -> [{atom()}]",
                        lists:concat(["% @doc Retrieve a list of the names of `belongs_to' associations."])])],
            erl_syntax:function(
                erl_syntax:atom(belongs_to_names),
                [erl_syntax:clause([], none, [erl_syntax:list(lists:map(
                                    fun(P) -> erl_syntax:atom(P) end, BelongsToList))])])),
    
    erl_syntax:add_precomments([erl_syntax:comment(
                ["% @spec belongs_to() -> [{atom(), BossRecord}]",
                    lists:concat(["% @doc Retrieve all of the `belongs_to' associations at once."])])],
        erl_syntax:function(
            erl_syntax:atom(belongs_to),
            [erl_syntax:clause([], none, [erl_syntax:list(lists:map(
                                fun(P) -> erl_syntax:tuple([
                                                erl_syntax:atom(P),
                                                erl_syntax:application(none, erl_syntax:atom(P), [])]) end, BelongsToList))])]))
            ].

attribute_names_forms(ModuleName, Parameters) ->
    [ erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec attribute_names() -> [atom()]",
                        lists:concat(["% @doc A list of the lower-case `", ModuleName, "' parameters."])])],
            erl_syntax:function(
                erl_syntax:atom(attribute_names),
                [erl_syntax:clause([], none, [erl_syntax:list(lists:map(
                                    fun(P) -> erl_syntax:atom(parameter_to_colname(P)) end,
                                    Parameters))])]))].

has_many_forms(HasMany, ModuleName) ->
    has_many_forms(HasMany, ModuleName, 1000000).

has_many_forms(HasMany, ModuleName, Limit) ->
    has_many_forms(HasMany, ModuleName, Limit, primary).

has_many_forms(HasMany, ModuleName, Limit, Sort) ->
    has_many_forms(HasMany, ModuleName, Limit, Sort, str_ascending).

has_many_forms(HasMany, ModuleName, Limit, Sort, SortOrder) ->
    Type = inflector:singularize(atom_to_list(HasMany)),
    [erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec "++atom_to_list(HasMany)++"() -> [ "++Type++" ]",
                        lists:concat(["% @doc Retrieves ", HasMany, " with `", ModuleName, "_id' ",
                                "set to the `Id' of this `", ModuleName, "'"])])],
            erl_syntax:function(erl_syntax:atom(HasMany),
                [erl_syntax:clause([], none, [
                            has_many_application_forms(Type, ModuleName, Limit, Sort, SortOrder)
                        ])])),
        erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec first_"++Type++"() -> "++Type,
                        lists:concat(["% @doc Retrieves the first `", Type, 
                                "' that would be returned by `", HasMany, "()'"])])],
            erl_syntax:function(erl_syntax:atom("first_"++Type),
                [erl_syntax:clause([], none, [
                            erl_syntax:application(
                                erl_syntax:atom(erlang),
                                erl_syntax:atom(hd), [ 
                            has_many_application_forms(Type, ModuleName, 1, Sort, SortOrder)
                        ])])])),
        erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec last_"++Type++"() -> "++Type,
                        lists:concat(["% @doc Retrieves the last `", Type,
                                "' that would be returned by `", HasMany, "()'"])])],
            erl_syntax:function(erl_syntax:atom("last_"++Type),
                [erl_syntax:clause([], none, [
                            erl_syntax:application(
                                erl_syntax:atom(erlang),
                                erl_syntax:atom(hd), [ 
                                    has_many_application_forms(Type, ModuleName, 1, Sort, reverse_sort_order(SortOrder))
                                ])])]))
    ].

reverse_sort_order(str_ascending) -> str_descending;
reverse_sort_order(str_descending) -> str_ascending;
reverse_sort_order(num_ascending) -> num_descending;
reverse_sort_order(num_descending) -> num_ascending.

has_many_application_forms(Type, ModuleName, Limit, Sort, SortOrder) ->
    erl_syntax:application(
        erl_syntax:atom(?DATABASE_MODULE), 
        erl_syntax:atom(find),
        [erl_syntax:atom(Type),
            erl_syntax:list([
                    erl_syntax:tuple([
                            erl_syntax:atom(
                                atom_to_list(ModuleName) ++ "_id"),
                            erl_syntax:variable("Id")])
                ]),
            erl_syntax:integer(Limit),
            erl_syntax:integer(0),
            erl_syntax:atom(Sort),
            erl_syntax:atom(SortOrder)
        ]).

belongs_to_forms(BelongsTo, ModuleName) ->
    erl_syntax:add_precomments([erl_syntax:comment(
                [lists:concat(["% @spec ", BelongsTo, "() -> ", 
                            inflector:camelize(atom_to_list(BelongsTo))]),
                    lists:concat(["% @doc Retrieves the ", BelongsTo, 
                            " with `Id' equal to the `", 
                            inflector:camelize(atom_to_list(BelongsTo)), "Id'",
                           " of this ", ModuleName])])],
        erl_syntax:function(erl_syntax:atom(BelongsTo),
            [erl_syntax:clause([], none, [
                        erl_syntax:application(
                            erl_syntax:atom(?DATABASE_MODULE),
                            erl_syntax:atom(find),
                            [erl_syntax:variable(inflector:camelize(atom_to_list(BelongsTo)) ++ "Id")]
                        )])])).

counter_getter_forms(Counters) ->
    lists:map(
        fun(Counter) ->
                erl_syntax:add_precomments([erl_syntax:comment(
                            ["% @spec "++atom_to_list(Counter)++"() -> integer()",
                                "% @doc Retrieve the value of the `"++atom_to_list(Counter)++"' counter"])],
                    erl_syntax:function(erl_syntax:atom(Counter),
                        [erl_syntax:clause([], none, [
                                    erl_syntax:application(
                                        erl_syntax:atom(?DATABASE_MODULE),
                                        erl_syntax:atom(counter),
                                        [erl_syntax:infix_expr(
                                                erl_syntax:variable("Id"),
                                                erl_syntax:operator("++"),
                                                erl_syntax:string("-counter-" ++ atom_to_list(Counter))
                                            )])])])) end, Counters).

counter_reset_forms([]) ->
    [];
counter_reset_forms(Counters) ->
    [erl_syntax:add_precomments([erl_syntax:comment(
                ["% @spec reset( Counter::atom() ) -> ok | {error, Reason}",
                    "% @doc Reset a counter to zero"])],
        erl_syntax:function(erl_syntax:atom(reset),
            lists:map(
                fun(Counter) ->
                        erl_syntax:clause([erl_syntax:atom(Counter)], none, [
                                erl_syntax:application(
                                    erl_syntax:atom(?DATABASE_MODULE),
                                    erl_syntax:atom(delete),
                                    [counter_name_forms(Counter)])])
                end, Counters)))].

counter_incr_forms([]) ->
    [];
counter_incr_forms(Counters) ->
    [ erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec incr( Counter::atom() ) -> integer()",
                        "@doc Atomically increment a counter by 1."])],
            erl_syntax:function(erl_syntax:atom(incr),
                lists:map(
                    fun(Counter) ->
                            erl_syntax:clause([erl_syntax:atom(Counter)], none, [
                                    erl_syntax:application(
                                        erl_syntax:atom(?DATABASE_MODULE),
                                        erl_syntax:atom(incr),
                                        [counter_name_forms(Counter)])]) 
                    end, Counters))),
        erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec incr( Counter::atom(), Increment::integer() ) ->"++
                        " integer()",
                        "% @doc Atomically increment a counter by the specified increment"])],
            erl_syntax:function(erl_syntax:atom(incr),
                lists:map(
                    fun(Counter) ->
                            erl_syntax:clause([erl_syntax:atom(Counter),
                                    erl_syntax:variable("Amount")], none, [
                                    erl_syntax:application(
                                        erl_syntax:atom(?DATABASE_MODULE),
                                        erl_syntax:atom(incr),
                                        [counter_name_forms(Counter),
                                            erl_syntax:variable("Amount")])])
                    end, Counters)))].

counter_name_forms(CounterVariable) ->
    erl_syntax:infix_expr(
        erl_syntax:infix_expr(
            erl_syntax:variable("Id"),
            erl_syntax:operator("++"),
            erl_syntax:string("-counter-")),
        erl_syntax:operator("++"),
        erl_syntax:application(
            erl_syntax:atom('erlang'),
            erl_syntax:atom('atom_to_list'),
            [erl_syntax:atom(CounterVariable)])).

parameter_to_colname(Parameter) when is_atom(Parameter) ->
    string:to_lower(inflector:underscore(atom_to_list(Parameter))).
