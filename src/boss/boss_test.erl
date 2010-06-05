% web-centric functional tests
-module(boss_test).
-compile(export_all).

start() ->
    boss_db:start([{driver, boss_db_driver_mock}]),
    boss_translator:start(),
    boss_controller:load_all_modules(),
    put(boss_environment, testing),
    Result = run_tests(),
    io:format("Result: ~p~n", [Result]),
    erlang:halt().

run_tests() ->
    admin_test:root_test().

get_request(Url, Headers, AssertionFun, Continuations) ->
    io:format("GET ~p~n", [Url]),
    RequesterPid = spawn(fun get_request_loop/0),
    RequesterPid ! {self(), Url, Headers},
    receive_response(RequesterPid, AssertionFun, Continuations).

post_request(Url, Headers, Contents, AssertionFun, Continuations) ->
    io:format("POST ~p~n~n~p~n", [Url, Contents]),
    RequesterPid = spawn(fun post_request_loop/0),
    RequesterPid ! {self(), Url, Headers, Contents},
    receive_response(RequesterPid, AssertionFun, Continuations).

follow_link(LinkName, {_, _, _, ParseTree}, AssertionFun, Continuations) ->
    case find_link_with_text(LinkName, ParseTree) of
        undefined -> 
            {0, ["No link to follow!"]};
        Url -> get_request(binary_to_list(Url), [], AssertionFun, Continuations)
    end.

submit_form(FormName, FormValues, {_, Uri, _, ParseTree}, AssertionFun, Continuations) ->
    case find_form_named(FormName, ParseTree) of
        undefined -> 
            {0, ["No form to submit!"]};
        {Method, Action, InputFields, InputLabels} ->
            FormAction = case Action of undefined -> Uri; Action -> binary_to_list(Action) end,
            EncodedForm = fill_out_form(InputFields, InputLabels, FormValues),
            case Method of
                <<"post">> ->
                    post_request(FormAction, [], EncodedForm, AssertionFun, Continuations);
                _ ->
                    Url = lists:concat([FormAction, "?", EncodedForm]),
                    get_request(Url, [], AssertionFun, Continuations)
            end
    end.

fill_out_form(InputFields, InputLabels, FormValues) ->
    MergedForm = lists:map(
        fun({Id, Name, DefaultValue}) ->
                case proplists:get_value(Id, InputLabels) of
                    undefined -> {Name, DefaultValue};
                    Label ->
                        LabelString = binary_to_list(Label),
                        case proplists:get_value(LabelString, FormValues) of
                            undefined -> {Name, DefaultValue};
                            Value -> {Name, Value}
                        end
                end
        end, InputFields),
    mochiweb_util:urlencode(MergedForm).

find_link_with_text(LinkName, ParseTree) when is_list(LinkName) ->
    find_link_with_text(list_to_binary(LinkName), ParseTree);
find_link_with_text(LinkName, ParseTree) when is_tuple(ParseTree) ->
    find_link_with_text(LinkName, [ParseTree]);
find_link_with_text(_LinkName, []) ->
    undefined;
find_link_with_text(LinkName, [Text|Rest]) when is_binary(Text) ->
    find_link_with_text(LinkName, Rest);
find_link_with_text(LinkName, [{<<"a">>, Attrs, [LinkName]}|Rest]) ->
    case proplists:get_value(<<"href">>, Attrs) of
        undefined -> find_link_with_text(LinkName, Rest);
        Url -> Url
    end;
find_link_with_text(LinkName, [{<<"a">>, Attrs, [{<<"img">>, ImgAttrs, []}]}|Rest]) ->
    case proplists:get_value(<<"href">>, Attrs) of
        undefined -> find_link_with_text(LinkName, Rest);
        Url ->
            case proplists:get_value(<<"alt">>, ImgAttrs) of
                LinkName -> Url;
                _ -> find_link_with_text(LinkName, Rest)
            end
    end;
find_link_with_text(LinkName, [{_OtherTag, _Attrs, []}|Rest]) ->
    find_link_with_text(LinkName, Rest);
find_link_with_text(LinkName, [{_OtherTag, _Attrs, Children}|Rest]) when is_list(Children) ->
    case find_link_with_text(LinkName, Children) of
        undefined -> find_link_with_text(LinkName, Rest);
        Url -> Url
    end.

find_form_named(FormName, ParseTree) when is_list(FormName) ->
    find_form_named(list_to_binary(FormName), ParseTree);
find_form_named(FormName, ParseTree) when not is_list(ParseTree) ->
    find_form_named(FormName, [ParseTree]);
find_form_named(_FormName, []) ->
    undefined;
find_form_named(FormName, [Text|Rest]) when is_binary(Text) ->
    find_form_named(FormName, Rest);
find_form_named(FormName, [{<<"form">>, Attrs, Children}|Rest]) ->
    case proplists:get_value(<<"name">>, Attrs) of
        undefined -> find_form_named(FormName, Rest);
        FormName -> process_form(Attrs, Children)
    end;
find_form_named(FormName, [{_OtherTag, _Attrs, Children}|Rest]) ->
    case find_form_named(FormName, Children) of
        undefined -> find_form_named(FormName, Rest);
        FoundIt -> FoundIt
    end.

process_form(Attrs, Children) when is_list(Attrs) ->
    Method = proplists:get_value(<<"method">>, Attrs),
    Action = proplists:get_value(<<"action">>, Attrs),
    {Inputs, Labels} = process_form_elements(Children),
    {Method, Action, Inputs, Labels}.

process_form_elements(Children) ->
    {InputAcc, LabelAcc} = process_form_elements(Children, [], []),
    {lists:reverse(InputAcc), lists:reverse(LabelAcc)}.

process_form_elements([], InputAcc, LabelAcc) ->
    {InputAcc, LabelAcc};
process_form_elements([Text|Rest], InputAcc, LabelAcc) when is_binary(Text) ->
    process_form_elements(Rest, InputAcc, LabelAcc);
process_form_elements([{<<"input">>, Attrs, []}|Rest], InputAcc, LabelAcc) ->
    Name = proplists:get_value(<<"name">>, Attrs),
    Id = proplists:get_value(<<"id">>, Attrs),
    Value = proplists:get_value(<<"value">>, Attrs),
    process_form_elements(Rest, [{Id, Name, Value}|InputAcc], LabelAcc);
process_form_elements([{<<"textarea">>, Attrs, [Value]}|Rest], InputAcc, LabelAcc) ->
    Name = proplists:get_value(<<"name">>, Attrs),
    Id = proplists:get_value(<<"id">>, Attrs),
    process_form_elements(Rest, [{Id, Name, Value}|InputAcc], LabelAcc);
process_form_elements([{<<"select">>, Attrs, Children}|Rest], InputAcc, LabelAcc) ->
    Name = proplists:get_value(<<"name">>, Attrs),
    Id = proplists:get_value(<<"id">>, Attrs),
    Value = find_selected_value(Children),
    process_form_elements(Rest, [{Id, Name, Value}|InputAcc], LabelAcc);
process_form_elements([{<<"label">>, Attrs, [LabelName]}|Rest], InputAcc, LabelAcc) when is_binary(LabelName) ->
    ForElement = proplists:get_value(<<"for">>, Attrs),
    process_form_elements(Rest, InputAcc, [{ForElement, LabelName}|LabelAcc]);
process_form_elements([{_OtherTag, _Attrs, Children}|Rest], InputAcc, LabelAcc) ->
    {InputAcc1, LabelAcc1} = process_form_elements(Children, InputAcc, LabelAcc),
    process_form_elements(Rest, InputAcc1, LabelAcc1).

find_selected_value([{<<"optgroup">>, _Attrs, Children}|Rest]) ->
    case find_selected_value(Children) of
        undefined -> find_selected_value(Rest);
        Selected -> Selected
    end;
find_selected_value([{<<"option">>, Attrs, [Label]}|Rest]) when is_binary(Label) ->
    case proplists:get_value(<<"selected">>, Attrs) of
        undefined -> find_selected_value(Rest);
        _ ->
            case proplists:get_value(<<"value">>, Attrs) of
                undefined -> Label;
                Value -> Value
            end
    end.

get_request_loop() ->
    receive
        {From, Uri, Headers} ->
            Req = make_request('GET', Uri, Headers),
            From ! {self(), Uri, boss_controller:process_request(Req)};
        Other ->
            error_logger:error_msg("Unexpected message in get_request_loop: ~p~n", [Other])
    end.

post_request_loop() ->
    receive
        {From, Uri, Headers, Body} ->
            erlang:put(mochiweb_request_body, Body),
            erlang:put(mochiweb_request_body_length, length(Body)),
            erlang:put(mochiweb_request_post, mochiweb_util:parse_qs(Body)),
            Req = make_request('POST', Uri, [{"Content-Encoding", "application/x-www-form-urlencoded"} | Headers]),
            From ! {self(), Uri, boss_controller:process_request(Req)};
        Other ->
            error_logger:error_msg("Unexpected message in post_request_loop: ~p~n", [Other])
    end.


make_request(Method, Uri, Headers) ->
    Req = mochiweb_request:new(
        false, %Socket
        Method, Uri, {1, 0}, mochiweb_headers:make(Headers)),
    DocRoot = "./static",
    simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}).

receive_response(RequesterPid, AssertionFun, Continuations) ->
    receive
        {RequesterPid, Uri, {Status, ResponseHeaders, ResponseBody}} ->
            ParsedResponseBody = case ResponseBody of
                [] -> [];
                Other -> mochiweb_html:parse(Other)
            end,
            ParsedResponse = {Status, Uri, ResponseHeaders, ParsedResponseBody},
            {NumSuccesses, FailureMessages} = lists:foldl(fun
                    ({true, _Msg}, {N, Acc}) ->
                        {N+1, Acc};
                    ({false, Msg}, {N, Acc}) ->
                        {N, [Msg|Acc]}
                end, {0, []}, AssertionFun(ParsedResponse)),
            io:format("Successes: ~p~nFailures: ~p~n", [NumSuccesses, FailureMessages]),
            exit(RequesterPid, kill),
            case length(FailureMessages) of
                0 ->
                    {NewS, NewF} = process_continuations(Continuations, ParsedResponse),
                    {NumSuccesses + NewS, FailureMessages ++ NewF};
                _ ->
                    {NumSuccesses, FailureMessages}
            end;
        Other ->
            error_logger:error_msg("Unexpected message in receive_response: ~p~n", [Other]),
            receive_response(RequesterPid, AssertionFun, Continuations)
    end.

assert_http_ok({Status, _, _, _}) ->
  {Status =:= 200, "HTTP Status not OK"}.

assert_http_redirect({Status, _, _, _}) ->
  {Status =:= 302, "HTTP Status not OK"}.

process_continuations(Continuations, Response) ->
    process_continuations(Continuations, Response, {0, []}).

process_continuations([], _, {NumSuccesses, FailureMessages}) ->
    {NumSuccesses, lists:reverse(FailureMessages)};
process_continuations([Name, Fun | Rest], Response, {NumSuccesses, FailureMessages}) 
        when is_list(Name) and is_function(Fun) ->
    io:format("Running test: ~p~n", [Name]),
    boss_db_driver_mock:push(),
    {TheseSuccesses, TheseFailureMessages} = Fun(Response),
    boss_db_driver_mock:pop(),
    process_continuations(Rest, Response, {NumSuccesses + TheseSuccesses, TheseFailureMessages ++ FailureMessages}).

