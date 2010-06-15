% web-centric functional tests
-module(boss_test).
-export([start/0, run_tests/0, get_request/4, post_request/5, read_email/4]).
-export([follow_link/4, follow_redirect/3, submit_form/5]).
-export([find_link_with_text/2]).

start() ->
    boss_db:start([{driver, boss_db_driver_mock}]),
    boss_mail:start([{driver, boss_mail_driver_mock}]),
    boss_translator:start(),
    boss_load:load_all_modules(),
    put(boss_environment, testing),
    run_tests(),
    erlang:halt().

run_tests() ->
    lists:map(fun(TestModule) ->
                TestModuleAtom = list_to_atom(TestModule),
                io:format("~nRunning: ~p~n", [TestModule]),
                io:format("~-60s", ["Root test"]),
                {NumSuccesses, FailureMessages} = TestModuleAtom:start(),
                io:format("~70c~n", [$=]),
                io:format("Passed: ~p~n", [NumSuccesses]),
                io:format("Failed: ~p~n", [length(FailureMessages)])
        end, boss_files:test_list()).

%% @spec get_request(Url, Headers, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test issues an HTTP GET request to `Url' (a path such as "/"
%% -- no "http://" or domain name), passing in `RequestHeaders' to the
%% server.
get_request(Url, Headers, Assertions, Continuations) ->
    RequesterPid = spawn(fun get_request_loop/0),
    RequesterPid ! {self(), Url, Headers},
    receive_response(RequesterPid, Assertions, Continuations).

%% @spec post_request(Url, Headers, Contents, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test issues an HTTP POST request to `Url' (a path such as "/"
%% -- no "http://" or domain name), passing in `RequestHeaders' to the
%% server, and `Contents' as the request body.
post_request(Url, Headers, Contents, Assertions, Continuations) ->
    RequesterPid = spawn(fun post_request_loop/0),
    RequesterPid ! {self(), Url, Headers, Contents},
    receive_response(RequesterPid, Assertions, Continuations).

%% @spec follow_link(LinkName, Response, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test looks for a link labeled `LinkName' in `Response' and issues
%% an HTTP GET request to the associated URL. The label may be an "alt" attribute of a 
%% hyperlinked &amp;lt;img&amp;gt; tag.
follow_link(LinkName, {_, Uri, _, ParseTree} = _Response, Assertions, Continuations) ->
    follow_link1(LinkName, Uri, ParseTree, Assertions, Continuations);
follow_link(LinkName, {_, _, ParseTree} = _Response, Assertions, Continuations) ->
    follow_link1(LinkName, "", ParseTree, Assertions, Continuations).

%% @spec follow_redirect(Response, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test follows an HTTP redirect; that is, it issues a GET request to
%% the URL specified by the "Location" header in `Response'
follow_redirect({302, _, Headers, _} = _Response, Assertions, Continuations) ->
  case proplists:get_value("Location", Headers) of
    undefined ->
      {0, ["No Location: header to follow!"]};
    Url ->
      get_request(Url, [], Assertions, Continuations)
  end.

%% @spec submit_form(FormName, FormValues::proplist(), Response, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test inspects `Response' for an HTML form with a "name" attribute equal to `FormName',
%% and submits it using `FormValues', a proplist with keys equal to the labels of form fields. 
%% (So all visible form fields should be labeled with a &amp;lt;label&amp;gt; HTML tag!)
%% If a particular value is not specified, the form's default value is used.
submit_form(FormName, FormValues, {_, Uri, _, ParseTree} = _Response, Assertions, Continuations) ->
    case find_form_named(FormName, ParseTree) of
        undefined -> 
            {0, ["No form to submit!"]};
        {Method, Action, InputFields, InputLabels} ->
            FormAction = case Action of undefined -> Uri; Action -> binary_to_list(Action) end,
            EncodedForm = fill_out_form(InputFields, InputLabels, FormValues),
            case Method of
                <<"post">> ->
                    post_request(FormAction, [], EncodedForm, Assertions, Continuations);
                _ ->
                    Url = lists:concat([FormAction, "?", EncodedForm]),
                    get_request(Url, [], Assertions, Continuations)
            end
    end.

%% @spec read_email(ToAddress, Subject, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test retrieves the most recent email sent by the application to `ToAddress' with
%% subject equal to `Subject'.
read_email(ToAddress, Subject, Assertions, Continuations) ->
    case boss_mail_driver_mock:read(ToAddress, Subject) of
        undefined ->
            {0, ["No such message was sent!"]};
        {Type, SubType, Headers, _, Body} ->
            {TextBody, HtmlBody} = parse_email_body(Type, SubType, Body),
            process_assertions_and_continuations(Assertions, Continuations, {Headers, TextBody, HtmlBody})
    end.

% Internal

follow_link1(LinkName, ThisUrl, ParseTree, Assertions, Continuations) ->
    case find_link_with_text(LinkName, ParseTree) of
        undefined -> 
            {0, ["No link to follow!"]};
        Url ->
            AbsPath = case parse_url(Url) of
                [$/|_] = AbsUrl ->
                    AbsUrl;
                Other ->
                    Dirname = filename:dirname(ThisUrl),
                    filename:join([Dirname, Other])
            end,
            get_request(AbsPath, [], Assertions, Continuations)
    end.

parse_email_body(<<"text">>, <<"plain">>, Body) ->
    {Body, []};
parse_email_body(<<"text">>, <<"html">>, Body) ->
    ParsedBody = mochiweb_html:parse(Body),
    {[], ParsedBody};
parse_email_body(<<"multipart">>, <<"alternative">>, 
    [{<<"text">>, <<"plain">>, _, _, TextBody}, 
        {<<"text">>, <<"html">>, _, _, HtmlBody}]) ->
    ParsedHtmlBody = mochiweb_html:parse(<<"<html>", HtmlBody/binary, "</html>">>), %hack
    {TextBody, ParsedHtmlBody}.

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
find_link_with_text(LinkName, [{<<"a">>, Attrs, [LinkName]}|_Rest]) ->
    proplists:get_value(<<"href">>, Attrs);
find_link_with_text(LinkName, [{<<"a">>, Attrs, [{<<"img">>, ImgAttrs, []}]}|Rest]) ->
    case proplists:get_value(<<"alt">>, ImgAttrs) of
        LinkName -> proplists:get_value(<<"href">>, Attrs);
        _ -> find_link_with_text(LinkName, Rest)
    end;
find_link_with_text(LinkName, [{<<"a">>, Attrs, Children}|Rest]) ->
    case flatten_html(Children) of
        LinkName -> proplists:get_value(<<"href">>, Attrs);
        _ -> find_link_with_text(LinkName, Rest)
    end;
find_link_with_text(LinkName, [{_OtherTag, _Attrs, []}|Rest]) ->
    find_link_with_text(LinkName, Rest);
find_link_with_text(LinkName, [{_OtherTag, _Attrs, Children}|Rest]) when is_list(Children) ->
    case find_link_with_text(LinkName, Children) of
        undefined -> find_link_with_text(LinkName, Rest);
        Url -> Url
    end.

parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url(Url) ->
    {_, _, Path, Query, Frag} = mochiweb_util:urlsplit(Url),
    mochiweb_util:urlunsplit_path({Path, Query, Frag}).

flatten_html(Children) ->
    iolist_to_binary(lists:reverse(flatten_html1(Children, []))).

flatten_html1([], Acc) ->
    lists:reverse(Acc);
flatten_html1([Text|Rest], Acc) when is_binary(Text) ->
    flatten_html1(Rest, [Text|Acc]);
flatten_html1([{_, _, Children}|Rest], Acc) ->
    [flatten_html(Rest), flatten_html(Children) | Acc].

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
            From ! {self(), Uri, boss_web_controller:process_request(Req)};
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
            From ! {self(), Uri, boss_web_controller:process_request(Req)};
        Other ->
            error_logger:error_msg("Unexpected message in post_request_loop: ~p~n", [Other])
    end.


make_request(Method, Uri, Headers) ->
    Req = mochiweb_request:new(
        false, %Socket
        Method, Uri, {1, 0}, mochiweb_headers:make(Headers)),
    DocRoot = "./static",
    simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}).

receive_response(RequesterPid, Assertions, Continuations) ->
    receive
        {RequesterPid, Uri, {Status, ResponseHeaders, ResponseBody}} ->
            ParsedResponseBody = case ResponseBody of
                [] -> [];
                Other -> mochiweb_html:parse(Other)
            end,
            exit(RequesterPid, kill),
            ParsedResponse = {Status, Uri, ResponseHeaders, ParsedResponseBody},
            process_assertions_and_continuations(Assertions, Continuations, ParsedResponse);
        _ ->
            receive_response(RequesterPid, Assertions, Continuations)
    end.

process_assertions_and_continuations(Assertions, Continuations, ParsedResponse) ->
    {NumSuccesses, FailureMessages} = process_assertions(Assertions, ParsedResponse),
    case length(FailureMessages) of
        0 ->
            io:format("~3B passed~n", [NumSuccesses]),
            {NewS, NewF} = process_continuations(Continuations, ParsedResponse),
            {NumSuccesses + NewS, FailureMessages ++ NewF};
        N ->
            io:format("~c[01;31m~3B failed~c[00m~n", [16#1B, N, 16#1B]),
            lists:map(fun(Msg) ->
                        io:format("~s* ~c[01m~p~c[00m~n", 
                            [lists:duplicate(boss_db_driver_mock:depth() - 1, $\ ), 
                                16#1B, Msg, 16#1B])
                end, FailureMessages),
            {NumSuccesses, FailureMessages}
    end.

process_assertions(Assertions, ParsedResponse) ->
    lists:foldl(fun
            (AssertionFun, {N, Acc}) when is_function(AssertionFun) ->
                case AssertionFun(ParsedResponse) of
                    {true, _Msg} ->
                        {N+1, Acc};
                    {false, Msg} ->
                        {N, [Msg|Acc]}
                end
        end, {0, []}, Assertions).

process_continuations(Continuations, Response) ->
    process_continuations(Continuations, Response, {0, []}).

process_continuations([], _, {NumSuccesses, FailureMessages}) ->
    {NumSuccesses, lists:reverse(FailureMessages)};
process_continuations([Name, Fun | Rest], Response, {NumSuccesses, FailureMessages}) 
        when is_list(Name) and is_function(Fun) ->
    io:format("~-60s", [lists:duplicate(boss_db_driver_mock:depth(), $\ ) ++ Name]),
    boss_db_driver_mock:push(),
    boss_mail_driver_mock:push(),
    {TheseSuccesses, TheseFailureMessages} = Fun(Response),
    boss_mail_driver_mock:pop(),
    boss_db_driver_mock:pop(),
    process_continuations(Rest, Response, {NumSuccesses + TheseSuccesses, TheseFailureMessages ++ FailureMessages}).

