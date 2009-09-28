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
            case compile_to_binary(trick_out_forms(Forms), Options) of
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
            case proplists:get_value(error, Forms) of
                undefined ->
                    {ok, Forms};
                {Line, _Module, ErrorDescription} ->
                    {error, {File, Line, ErrorDescription}}
            end;
        Error ->
            Error
    end.

compile_to_binary(Forms, Options) ->
    case compile:forms(erl_syntax:revert_forms(Forms), 
            proplists:get_value(compiler_options, Options, [verbose, return_errors])) of
        {ok, Module1, Bin} ->
            code:purge(Module1),
            case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
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

    lists:reverse(OtherForms) ++ 
        association_forms(ModuleName, Attributes) ++
        counter_getter_forms(Counters) ++
        counter_reset_forms(Counters) ++
        counter_incr_forms(Counters) ++
        save_forms(ModuleName, Parameters) ++
        set_attributes_forms(ModuleName, Parameters) ++
        get_attributes_forms(ModuleName, Parameters) ++
        attribute_names_forms(ModuleName, Parameters) ++
        parameter_getter_forms(Parameters) ++
        parameter_setter_forms(ModuleName, Parameters) ++
        [].

save_forms(ModuleName, Parameters) ->
    [erl_syntax:add_precomments([erl_syntax:comment(
                    [lists:concat(["% @spec save() -> Saved", inflector:camelize(atom_to_list(ModuleName))]),
                        lists:concat(["% @doc Saves this `", ModuleName, "' record to the database. The returned record"]),
                        "% will have an auto-generated ID if the record's ID was set to 'id'."])],
            erl_syntax:function(
                erl_syntax:atom(save),
                [erl_syntax:clause([], none,
                        [erl_syntax:application(
                                erl_syntax:atom(?DATABASE_MODULE),
                                erl_syntax:atom(save_record),
                                [erl_syntax:tuple([erl_syntax:atom(ModuleName)|
                                            lists:map(fun(P) ->
                                                        erl_syntax:variable(P)
                                                end, Parameters)
                                        ])
                                ])])]))].

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
                [erl_syntax:clause([erl_syntax:variable("NewAttributes")], none,
                        [erl_syntax:application(
                                erl_syntax:atom(ModuleName),
                                erl_syntax:atom(new),
                                lists:map(fun(P) ->
                                            erl_syntax:application(
                                                erl_syntax:atom(proplists),
                                                erl_syntax:atom(get_value),
                                                [erl_syntax:atom(parameter_to_colname(P)),
                                                    erl_syntax:variable("NewAttributes"),
                                                    erl_syntax:variable(P)])
                                    end, Parameters))])]))].

association_forms(ModuleName, Attributes) ->
    lists:foldl(
        fun
            ({has_many, HasMany}, Acc) ->
                [has_many_forms(HasMany, ModuleName)|Acc];
            ({has_up_to, {Limit, HasMany}}, Acc) ->
                [has_many_forms(HasMany, ModuleName, Limit)|Acc];
            ({has_up_to, {Limit, HasMany, Sort}}, Acc) ->
                [has_many_forms(HasMany, ModuleName, Limit, Sort)|Acc];
            ({has_up_to, {Limit, HasMany, Sort, SortOrder}}, Acc) ->
                [has_many_forms(HasMany, ModuleName, Limit, Sort, SortOrder)|Acc];
            ({belongs_to, BelongsTo}, Acc) ->
                [belongs_to_forms(BelongsTo, ModuleName)|Acc];
            (_, Acc) ->
                Acc
        end, [], Attributes).

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
    erl_syntax:add_precomments([erl_syntax:comment(
                ["% @spec "++atom_to_list(HasMany)++"() -> [ "++Type++" ]",
                    lists:concat(["% @doc Retrieves ", HasMany, " with `", ModuleName, "_id' ",
                            "set to the `Id' of this `", ModuleName, "'"])])],
        erl_syntax:function(erl_syntax:atom(HasMany),
            [erl_syntax:clause([], none, [ 
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
                            ])])])).

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
                    ["% @spec incr( CounterName::atom() ) -> integer()",
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
                    ["% @spec incr( CounterName::atom(), Increment::integer() ) ->"++
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
