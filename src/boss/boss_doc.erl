%% @author Evan Miller
%% @doc Generate documentation for the Chicago Boss external API.

-module(boss_doc).
-export([run/0, run/2]).
-include("xmerl.hrl").

run() ->
    run("doc-src", "doc").

run(InDir, OutDir) ->
    file:make_dir(OutDir),
    {ok, Filenames} = file:list_dir(InDir),
    {TemplateModules, CompileErrors} = lists:foldl(fun(File, {ModAcc, ErrAcc}) ->
                case lists:suffix(".html", File) of
                    true ->
                        Module = module_name_from_template(File),
                        case erlydtl:compile(filename:join([InDir, File]), Module, [{out_dir, false}, {auto_escape, false}]) of
                            {ok, Module} -> {[{File, Module}|ModAcc], ErrAcc};
                            Error -> {ModAcc, [Error|ErrAcc]}
                        end;
                    false -> 
                        case File of
                            "."++_File -> ok;
                            File ->
                                {ok, _} = file:copy(filename:join([InDir, File]),
                                    filename:join([OutDir, File]))
                        end,
                        {ModAcc, ErrAcc}
                end
        end, {[], []}, Filenames),
    {TemplateOutputs, RenderErrors} = lists:foldl(fun({File, Module}, {OutputAcc, ErrAcc}) ->
                case Module:render([{this_page, File}|get_vars(File, InDir)]) of
                    {ok, IOList} -> {[{File, IOList}|OutputAcc], ErrAcc};
                    Error -> {OutputAcc, [Error|ErrAcc]}
                end
        end, {[], []}, TemplateModules),
    WriteErrors = lists:foldl(fun({File, Output}, Acc) ->
                case file:write_file(filename:join([OutDir, File]), Output) of
                    ok -> Acc;
                    Error -> [Error|Acc]
                end
        end, [], TemplateOutputs),
    io:format("Compile errors: ~w~n", [CompileErrors]),
    io:format("Render errors: ~w~n", [RenderErrors]),
    io:format("Write errors: ~w~n", [WriteErrors]),
    ok.

%% @spec module_name_from_template( File::string() ) -> Module::atom()
module_name_from_template(File) ->
    list_to_atom("boss_doc_template_"++filename:basename(File, ".html")).

%% @spec get_vars( Template::string() ) -> [{Key::atom(), Value}]
get_vars("api-view.html", _InDir) ->
    {erlydtl_filters, EDoc} = edoc:get_doc("deps/erlydtl/src/erlydtl_filters.erl", []),
    Functions = extract_function_docs(EDoc),
    [{filters, Functions}];
get_vars("api-record.html", InDir) ->
    %% NOTE: it is question if we really want to make edoc
    %% from trivial_boss_record by boss_model_manager or
    %% via boss_record_compiler...
    {boss_record, EDoc} = boss_record_compiler:edoc_module(filename:join([InDir, "trivial_boss_record.erl"]), 
        [{private, true}, {hidden, true}]),
    [{functions, extract_function_docs(EDoc)}];
get_vars("api-db.html", _InDir) ->
    {boss_db, EDoc} = edoc:get_doc("deps/boss_db/src/boss_db.erl", []),
    [{functions, extract_function_docs(EDoc)}];
get_vars("api-mq.html", _InDir) ->
    {boss_mq, EDoc} = edoc:get_doc("src/boss/boss_mq.erl", []),
    [{functions, extract_function_docs(EDoc)}];
get_vars("api-news.html", _InDir) ->
    {boss_news, EDoc} = edoc:get_doc("deps/boss_db/src/boss_news.erl", []),
    [{functions, extract_function_docs(EDoc)}];
get_vars("api-test.html", _InDir) ->
    {boss_web_test, EDoc1} = edoc:get_doc("src/boss/boss_web_test.erl", []),
    {boss_assert, EDoc2} = edoc:get_doc("src/boss/boss_assert.erl", []),
    [{test_functions, extract_function_docs(EDoc1)},
        {assert_functions, extract_function_docs(EDoc2)}];
get_vars("api-session.html", _InDir) ->
    {boss_session, EDoc1} = edoc:get_doc("src/boss/boss_session.erl", []),
    {boss_flash, EDoc2} = edoc:get_doc("src/boss/boss_flash.erl", []),
    [{session_functions, extract_function_docs(EDoc1)},
        {flash_functions, extract_function_docs(EDoc2)}];
get_vars(_, _InDir) ->
    [].

extract_function_docs(#xmlElement{name = module, content = Elements}) ->
    extract_function_docs(Elements);
extract_function_docs([#xmlElement{name = functions, content = Elements}|_Rest]) ->
    extract_function_docs(Elements);
extract_function_docs([#xmlElement{name = function, attributes = Attrs, content = Elements}|Rest]) ->
    [analyze_function_xml(Attrs, Elements)|extract_function_docs(Rest)];
extract_function_docs([_NotFunctions|Rest]) ->
    extract_function_docs(Rest);
extract_function_docs([]) ->
    [].

analyze_function_xml(
    [#xmlAttribute{name = name, value = Name},
        #xmlAttribute{name = arity, value = Arity}|_], Elements) ->
    analyze_function([{function, Name}, {arity, Arity}], Elements).

analyze_function(Attributes,
    [#xmlElement{name = description, content = DescriptionElements}|Rest]) ->
    analyze_function(Attributes, Rest) ++ analyze_function_description(DescriptionElements);
analyze_function(Attributes,
    [#xmlElement{name = typespec, content = TypeSpec}|Rest]) ->
    analyze_function(Attributes, Rest) ++ [{typespec, analyze_typespec(TypeSpec)}];
analyze_function(Attributes, [_Other|Rest]) ->
    analyze_function(Attributes, Rest);
analyze_function(Attributes, []) ->
    Attributes.

analyze_function_description(
    [#xmlElement{name = briefDescription, content = Nodes}|Rest]) ->
    [{description, analyze_description_nodes(Nodes)} | analyze_function_description(Rest)];
analyze_function_description(
    [#xmlElement{name = fullDescription, content = Nodes}|Rest]) ->
    [{description_long, analyze_description_nodes(Nodes)} | analyze_function_description(Rest)];
analyze_function_description([_|Rest]) ->
    analyze_function_description(Rest);
analyze_function_description([]) ->
    [].

analyze_description_nodes([#xmlText{ value = Text } | Rest]) ->
    Text ++ analyze_description_nodes(Rest);
analyze_description_nodes([#xmlElement{ name = code, content = Nodes }|Rest]) ->
    "<code>" ++ analyze_description_nodes(Nodes) ++ "</code>" ++ analyze_description_nodes(Rest);
analyze_description_nodes([#xmlElement{ name = p, content = Nodes }|Rest]) ->
    analyze_description_nodes(Nodes) ++ "<br /><br />" ++ analyze_description_nodes(Rest);
analyze_description_nodes([]) ->
    "".

analyze_typespec(
    [#xmlElement{ name = type, content = 
            [#xmlElement{ name = 'fun', content = 
                    [#xmlElement{ name = 'argtypes', content = ArgTypes },
                        #xmlElement{ name = 'type', content = Returns }]}]} | _Rest]) ->
    "(" ++ string:join(analyze_argtypes(ArgTypes), ", ") ++ ") -> " ++ analyze_argtypes_content(Returns);
analyze_typespec([_|Rest]) ->
    analyze_typespec(Rest);
analyze_typespec([]) ->
    "".

analyze_argtypes([#xmlElement{ name = 'type', attributes = Attrs, content = Content}|Rest]) ->
    AnalyzedAttrs = analyze_argtypes_attrs(Attrs),
    AnalyzedRest = analyze_argtypes(Rest),
    case analyze_argtypes_content(Content) of
        AnalyzedAttrs -> [AnalyzedAttrs | AnalyzedRest];
        AnalyzedContent -> [AnalyzedAttrs ++ AnalyzedContent | AnalyzedRest]
    end;
analyze_argtypes([_|Rest]) ->
    analyze_argtypes(Rest);
analyze_argtypes([]) ->
    [].

analyze_argtypes_attrs([#xmlAttribute{name = name, value = Name}|Rest]) ->
   Name ++ analyze_argtypes_attrs(Rest);
analyze_argtypes_attrs([#xmlAttribute{name = value, value = Name}|Rest]) ->
    Name ++ analyze_argtypes_attrs(Rest);
analyze_argtypes_attrs([_|Rest]) ->
    analyze_argtypes_attrs(Rest);
analyze_argtypes_attrs([]) ->
    "".

analyze_argtypes_content([#xmlElement{ name = 'typevar', attributes = 
            [#xmlAttribute{ name = 'name', value = Name }]}]) ->
    Name;
analyze_argtypes_content([#xmlElement{ name = 'list', content = 
            [#xmlElement{ name = 'type', content = Content }]}]) ->
    "["++analyze_argtypes_content(Content)++"]";
analyze_argtypes_content([#xmlElement{ name = 'tuple', content = Content}]) ->
    "{"++string:join(analyze_argtypes(Content), ", ")++"}";
analyze_argtypes_content([#xmlElement{ name = 'union', content = Content}]) ->
    string:join(analyze_argtypes(Content), " | ");
analyze_argtypes_content([#xmlElement{ name = 'atom', attributes = Attrs }]) ->
    analyze_argtypes_attrs(Attrs);
analyze_argtypes_content([#xmlElement{ name = 'abstype', content = 
            [#xmlElement{ name = erlangName, attributes = 
                    [#xmlAttribute{ name = 'name', value = Type }]}]}]) ->
    "<span class=\"typevar\">::" ++ Type ++ "()</span>";
analyze_argtypes_content([#xmlElement{ name = 'abstype', content =
            [#xmlElement{ name = erlangName, attributes =
                    [#xmlAttribute{ name = 'module', value = ModuleName },
                     #xmlAttribute{ name = 'name', value = Type }]}]}]) ->
    "<span class=\"typevar\">::" ++ ModuleName ++ ":" ++ Type ++ "()</span>".
