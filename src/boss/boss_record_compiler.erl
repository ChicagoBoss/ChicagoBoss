-module(boss_record_compiler).
-author('emmiller@gmail.com').
-define(DATABASE_MODULE, boss_db).
-define(PREFIX, "BOSSRECORDINTERNAL").
-define(HAS_MANY_LIMIT, 1000 * 1000 * 1000).

-export([compile/1, compile/2, edoc_module/1, edoc_module/2, trick_out_forms/1]).

%% @spec compile( File::string() ) -> {ok, Module} | {error, Reason}
%% @equiv compile(File, [])
compile(File) ->
    compile(File, []).

compile(File, Options) ->
    boss_compiler:compile(File, 
        [{pre_revert_transform, fun ?MODULE:trick_out_forms/1}|Options]).

%% @spec edoc_module( File::string() ) -> {Module::atom(), EDoc}
%% @equiv edoc_module(File, [])
edoc_module(File) ->
    edoc_module(File, []).

%% @spec edoc_module( File::string(), Options ) -> {Module::atom(), EDoc}
%% @doc Return an `edoc_module()' for the given Erlang source file when
%% compiled as a BossRecord.
edoc_module(File, Options) ->
    {ok, Forms} = boss_compiler:parse(File),
    edoc_extract:source(trick_out_forms(Forms), edoc:read_comments(File), 
        File, edoc_lib:get_doc_env([]), Options).

trick_out_forms(Forms) ->
    trick_out_forms(Forms, []).

trick_out_forms([
        {attribute, _Pos, module, {ModuleName, Parameters}} = H
        | Forms], LeadingForms) ->
    trick_out_forms(lists:reverse([H|LeadingForms]), Forms, ModuleName, Parameters);
trick_out_forms([H|T], LeadingForms) ->
    trick_out_forms(T, [H|LeadingForms]).

trick_out_forms(LeadingForms, Forms, ModuleName, Parameters) ->
    Attributes = proplists:get_value(attributes, erl_syntax_lib:analyze_forms(LeadingForms ++ Forms)),
    [{eof, _Line}|ReversedOtherForms] = lists:reverse(Forms),
    UserForms = lists:reverse(ReversedOtherForms),
    Counters = lists:foldl(
        fun
            ({counter, Counter}, Acc) -> [Counter|Acc];
            (_, Acc) -> Acc
        end, [], Attributes),

    GeneratedForms = 
        save_forms(ModuleName) ++
        validate_forms(ModuleName) ++
        set_attributes_forms(ModuleName, Parameters) ++
        get_attributes_forms(ModuleName, Parameters) ++
        attribute_names_forms(ModuleName, Parameters) ++
        counter_getter_forms(Counters) ++
        counter_reset_forms(Counters) ++
        counter_incr_forms(Counters) ++
        association_forms(ModuleName, Attributes) ++
        parameter_getter_forms(Parameters),

    UserFunctionList = list_functions(UserForms),
    GeneratedFunctionList = list_functions(GeneratedForms),

    GeneratedExportForms = export_forms(GeneratedFunctionList),

    LeadingForms ++ GeneratedExportForms ++ UserForms ++ 
        override_functions(GeneratedForms, UserFunctionList).

list_functions(Forms) ->
    list_functions(Forms, []).

list_functions([], DefinedFunctions) ->
    lists:reverse(DefinedFunctions);
list_functions([{'function', _, Name, Arity, _}|Rest], DefinedFunctions) ->
    list_functions(Rest, [{Name, Arity}|DefinedFunctions]);
list_functions([{tree, 'function', _, {'function', {tree, 'atom', _, Name}, 
                [{tree, 'clause', _, {'clause', Args, _, _}}|_]}}|Rest], 
    DefinedFunctions) ->
    Arity = length(Args), 
    list_functions(Rest, [{Name, Arity}|DefinedFunctions]);
list_functions([_H|T], DefinedFunctions) ->
    list_functions(T, DefinedFunctions).

override_functions(Forms, DefinedFunctions) ->
    override_functions(Forms, [], DefinedFunctions).

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

export_forms(FunctionList) ->
    export_forms(FunctionList, []).

export_forms([], Acc) ->
    lists:reverse(Acc);
export_forms([{Name, Arity}|Rest], Acc) ->
    export_forms(Rest, [erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(Arity))])])|Acc]).

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

get_attributes_forms(ModuleName, Parameters) ->
    [erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec attributes() -> [{Attribute::atom(), Value::string() | undefined}]",
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
                    ["% @spec set([{Attribute::atom(), Value}]) -> "++inflector:camelize(atom_to_list(ModuleName)),
                        "% @doc Set multiple record attributes at once. Does not save the record."])],
            erl_syntax:function(
                erl_syntax:atom(set),
                [erl_syntax:clause([erl_syntax:variable("AttributeProplist")], none,
                        [erl_syntax:application(
                                erl_syntax:atom(ModuleName),
                                erl_syntax:atom(new),
                                lists:map(fun(P) ->
                                            erl_syntax:application(
                                                erl_syntax:atom(proplists),
                                                erl_syntax:atom(get_value),
                                                [erl_syntax:atom(parameter_to_colname(P)),
                                                    erl_syntax:variable("AttributeProplist"),
                                                    erl_syntax:variable(P)])
                                    end, Parameters))])])),
    erl_syntax:add_precomments([erl_syntax:comment(
                    ["% @spec set(Attribute::atom(), NewValue) -> "++inflector:camelize(atom_to_list(ModuleName)),
                        "% @doc Set the value of a particular attribute. Does not save the record."])],
            erl_syntax:function(
                erl_syntax:atom(set),
                [erl_syntax:clause([erl_syntax:variable("Attribute"), erl_syntax:variable("NewValue")], none,
                        [
                            erl_syntax:application(
                                erl_syntax:atom(set),
                                [erl_syntax:list([erl_syntax:tuple([erl_syntax:variable("Attribute"), erl_syntax:variable("NewValue")])])])
                        ])]))
    ].

association_forms(ModuleName, Attributes) ->
    {Forms, BelongsToList} = lists:foldl(
        fun
            ({has, {HasOne, 1}}, {Acc, BT}) ->
                {has_one_forms(HasOne, ModuleName, []) ++ Acc, BT};
            ({has, {HasOne, 1, Opts}}, {Acc, BT}) ->
                {has_one_forms(HasOne, ModuleName, Opts) ++ Acc, BT};
            ({has, {HasMany, Limit}}, {Acc, BT}) ->
                {has_many_forms(HasMany, ModuleName, Limit, []) ++ Acc, BT};
            ({has, {HasMany, Limit, Opts}}, {Acc, BT}) ->
                {has_many_forms(HasMany, ModuleName, Limit, Opts) ++ Acc, BT};
            ({belongs_to, BelongsTo}, {Acc, BT}) ->
                {[belongs_to_forms(BelongsTo, BelongsTo, ModuleName)|Acc], [BelongsTo|BT]};
            ({OtherAttr, BelongsTo}, {Acc, BT}) ->
                case atom_to_list(OtherAttr) of
                    "belongs_to_"++Type ->
                        {[belongs_to_forms(Type, BelongsTo, ModuleName)|Acc], [BelongsTo|BT]};
                    _ ->
                        {Acc, BT}
                end;
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

has_one_forms(HasOne, ModuleName, Opts) ->
    Type = proplists:get_value(module, Opts, HasOne),
    ForeignKey = proplists:get_value(foreign_key, Opts, atom_to_list(ModuleName) ++ "_id"),
    [erl_syntax:add_precomments([erl_syntax:comment(
                    [lists:concat(["% @spec ", HasOne, "() -> ", Type, " | undefined"]),
                        lists:concat(["% @doc Retrieves the `", Type, "' with `", ForeignKey, "' ",
                                "set to the `Id' of this `", ModuleName, "'"])])],
            erl_syntax:function(erl_syntax:atom(HasOne),
                [erl_syntax:clause([], none, [
                            first_or_undefined_forms(
                                has_many_application_forms(Type, ForeignKey, 1, id, str_ascending)
                            )
                        ])]))
    ].

has_many_forms(HasMany, ModuleName, many, Opts) ->
    has_many_forms(HasMany, ModuleName, ?HAS_MANY_LIMIT, Opts);
has_many_forms(HasMany, ModuleName, Limit, Opts) -> 
    Sort = proplists:get_value(sort_by, Opts, 'id'),
    SortOrder = proplists:get_value(sort_order, Opts, str_ascending),
    Singular = inflector:singularize(atom_to_list(HasMany)),
    Type = proplists:get_value(module, Opts, Singular),
    ForeignKey = proplists:get_value(foreign_key, Opts, atom_to_list(ModuleName) ++ "_id"),
    [erl_syntax:add_precomments([erl_syntax:comment(
                    [
                        lists:concat(["% @spec ", HasMany, "() -> [ ", Type, " ]"]),
                        lists:concat(["% @doc Retrieves `", Type, "' records with `", ForeignKey, "' ",
                                "set to the `Id' of this `", ModuleName, "'"])])],
            erl_syntax:function(erl_syntax:atom(HasMany),
                [erl_syntax:clause([], none, [
                            has_many_application_forms(Type, ForeignKey, Limit, Sort, SortOrder)
                        ])])),
        erl_syntax:add_precomments([erl_syntax:comment(
                    [
                        lists:concat(["% @spec first_", Singular, "() -> ", Type, " | undefined"]),
                        lists:concat(["% @doc Retrieves the first `", Type, 
                                "' that would be returned by `", HasMany, "()'"])])],
            erl_syntax:function(erl_syntax:atom("first_"++Singular),
                [erl_syntax:clause([], none, [
                            first_or_undefined_forms(
                                has_many_application_forms(Type, ForeignKey, 1, Sort, SortOrder)
                            )
                        ])])),
        erl_syntax:add_precomments([erl_syntax:comment(
                    [
                        lists:concat(["% @spec last_", Singular, "() -> ", Type, " | undefined"]),
                        lists:concat(["% @doc Retrieves the last `", Type,
                                "' that would be returned by `", HasMany, "()'"])])],
            erl_syntax:function(erl_syntax:atom("last_"++Singular),
                [erl_syntax:clause([], none, [
                            first_or_undefined_forms(
                                    has_many_application_forms(Type, ForeignKey, 1, Sort, reverse_sort_order(SortOrder))
                                )
                        ])]))
    ].

first_or_undefined_forms(Forms) ->
    erl_syntax:case_expr(Forms,
        [erl_syntax:clause([erl_syntax:list([erl_syntax:variable(?PREFIX++"Record")])], none,
                [erl_syntax:variable(?PREFIX++"Record")]),
            erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(undefined)])]).

reverse_sort_order(str_ascending) -> str_descending;
reverse_sort_order(str_descending) -> str_ascending;
reverse_sort_order(num_ascending) -> num_descending;
reverse_sort_order(num_descending) -> num_ascending.

has_many_application_forms(Type, ForeignKey, Limit, Sort, SortOrder) ->
    erl_syntax:application(
        erl_syntax:atom(?DATABASE_MODULE), 
        erl_syntax:atom(find),
        [erl_syntax:atom(Type),
            erl_syntax:list([
                    erl_syntax:tuple([
                            erl_syntax:atom(ForeignKey),
                            erl_syntax:variable("Id")])
                ]),
            erl_syntax:integer(Limit),
            erl_syntax:integer(0),
            erl_syntax:atom(Sort),
            erl_syntax:atom(SortOrder)
        ]).

belongs_to_forms(Type, BelongsTo, ModuleName) ->
    erl_syntax:add_precomments([erl_syntax:comment(
                [lists:concat(["% @spec ", BelongsTo, "() -> ", 
                            inflector:camelize(atom_to_list(BelongsTo))]),
                    lists:concat(["% @doc Retrieves the ", Type, 
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
                            [
                                lists:concat(["% @spec ", Counter, "() -> integer()"]),
                                lists:concat(["% @doc Retrieve the value of the `", Counter, "' counter"])])],
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
