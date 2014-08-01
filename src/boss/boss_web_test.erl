% web-centric functional tests
-module(boss_web_test).
-export([start/1, run_tests/1, get_request/4, post_request/5, read_email/4]).
-export([follow_link/4, follow_redirect/3, follow_redirect/4, submit_form/5]).
-export([find_link_with_text/2]).

-include("boss_web.hrl").

start([Application]) ->
    start([Application, "mock"]);
start([Application, Adapter]) ->
    run_tests([Application, Adapter|boss_files_util:test_list(Application)]).

bootstrap_test_env(Application, Adapter) ->
    DBOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [db_port, db_host, db_username, db_password, db_database]),
    ok = application:start(Application),
    ControllerList     = boss_files:web_controller_list(Application),
    RouterAdapter     = boss_env:router_adapter(),
    {ok, RouterSupPid} = RouterAdapter:start([{application, Application}, 
            {controllers, ControllerList}]),
    boss_db:start([{adapter, Adapter}|DBOptions]),
    boss_session:start(),
    boss_mq:start(),
    lists:map(fun(File) ->
                {ok, _} = boss_compiler:compile(File, [{include_dirs, [boss_files_util:include_dir() | boss_env:get_env(boss, include_dirs, [])]}])
              end, boss_files:init_file_list(Application)),
    boss_news:start(),
    boss_mail:start([{driver, boss_mail_driver_mock}]),
    {ok, TranslatorSupPid} = boss_translator:start([{application, Application}]),
    boss_load:load_all_modules(Application, TranslatorSupPid),
    #boss_app_info{ 
        application		= Application,
        base_url		= "",
        init_data		= [],
        router_sup_pid		= RouterSupPid,
        translator_sup_pid	= TranslatorSupPid,
        model_modules		= boss_files:model_list(Application),
        controller_modules	= boss_files:web_controller_list(Application),
        view_modules		= boss_files:view_module_list(Application)
    }.

-spec(run_tests(_) -> no_return()).
% This function deliberately takes one argument so it can be invoked from the command-line.
run_tests([Application, Adapter|TestList]) ->
    AppInfo = bootstrap_test_env(list_to_atom(Application), list_to_atom(Adapter)),
    Pid = erlang:spawn(fun() -> app_info_loop(AppInfo) end),
    register(app_info, Pid),
    io:format("Found tests: ~p~n", [TestList]),
    lists:map(fun(TestModule) ->
                TestModuleAtom = list_to_atom(TestModule),
                io:format("~nRunning: ~p~n", [TestModule]),
                io:format("~-60s", ["Root test"]),
                {NumSuccesses, FailureMessages} = TestModuleAtom:start(),
                io:format("~70c~n", [$=]),
                io:format("Passed: ~p~n", [NumSuccesses]),
                io:format("Failed: ~p~n", [length(FailureMessages)])
        end, TestList),
    erlang:halt().

%% @spec get_request(Url, Headers, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test issues an HTTP GET request to `Url' (a path such as "/"
%% -- no "http://" or domain name), passing in `RequestHeaders' to the
%% server.
get_request(Url, Headers, Assertions, Continuations) ->
    app_info ! {self(), get_info},
    receive
        {_From, AppInfo} ->
            RequesterPid = spawn(fun() -> get_request_loop(AppInfo) end),
            link(RequesterPid),
            RequesterPid ! {self(), Url, Headers},
            receive_response(RequesterPid, Assertions, Continuations)
    end.

%% @spec post_request(Url, Headers, Contents, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test issues an HTTP POST request to `Url' (a path such as "/"
%% -- no "http://" or domain name), passing in `RequestHeaders' to the
%% server, and `Contents' as the request body.
post_request(Url, Headers, Contents, Assertions, Continuations) ->
    app_info ! {self(), get_info},
    receive
        {_From, AppInfo} ->
            RequesterPid = spawn(fun() -> post_request_loop(AppInfo) end),
            link(RequesterPid),
            RequesterPid ! {self(), Url, Headers, Contents},
            receive_response(RequesterPid, Assertions, Continuations)
    end.

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
follow_redirect(Response, Assertions, Continuations) ->
	follow_redirect(Response, [], Assertions, Continuations).

%% @spec follow_redirect(Response, Hdrs, Assertions, Continuations) -> [{NumPassed, ErrorMessages}]
%% @doc This test follows an HTTP redirect; that is, it issues a GET request to
%% the URL specified by the "Location" header in `Response', while passing `Hdrs' out as
%% outbound headers.
follow_redirect({302, _, Headers, _} = _Response, Hdrs, Assertions, Continuations) ->
  case proplists:get_value("Location", Headers) of
    undefined ->
      {0, ["No Location: header to follow!"]};
    Url ->
      get_request(Url, Hdrs, Assertions, Continuations)
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
    PushFun = fun() ->
            boss_db:push(),
            boss_mail_driver_mock:push()
    end,
    PopFun = fun() ->
            boss_db:pop(),
            boss_mail_driver_mock:pop()
    end,
    case boss_mail_driver_mock:read(ToAddress, Subject) of
        undefined ->
            boss_test:process_assertions_and_continuations(Assertions, Continuations, undefined, 
                PushFun, PopFun, fun boss_db:dump/0);
        {Type, SubType, Headers, _, Body} ->
            {TextBody, HtmlBody} = parse_email_body(Type, SubType, Body),
            boss_test:process_assertions_and_continuations(Assertions, Continuations, {Headers, TextBody, HtmlBody}, 
                PushFun, PopFun, fun boss_db:dump/0)
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
    {[], parse_html_email_body(Body)};
parse_email_body(<<"multipart">>, <<"alternative">>, 
    [{<<"text">>, <<"plain">>, _, _, TextBody}, 
        {<<"text">>, <<"html">>, _, _, HtmlBody}]) ->
    {TextBody, parse_html_email_body(HtmlBody)}.

parse_html_email_body(HtmlBody) -> %hack
    mochiweb_html:parse(<<"<html>", HtmlBody/binary, "</html>">>).

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
find_link_with_text(LinkName, [{_OtherTag, _Attrs}|Rest]) ->
    find_link_with_text(LinkName, Rest);
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

find_form_named(FormName, [{comment, _}|Rest]) ->
    find_form_named(FormName, Rest);
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

app_info_loop(AppInfo) ->
    receive
        {From, get_info} ->
            From ! {self(), AppInfo},
            app_info_loop(AppInfo)
    end.

get_request_loop(AppInfo) ->
    put(boss_environment, testing),
    receive
        {From, Uri, Headers} ->
            Req = make_request('GET', Uri, Headers),
            FullUrl = Req:path(),
            [{_, RouterPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
            [{_, TranslatorPid, _, _}] = supervisor:which_children(AppInfo#boss_app_info.translator_sup_pid),
			RouterAdapter = boss_env:router_adapter(),
            Result = boss_web_controller_handle_request:process_request(AppInfo#boss_app_info {
                    router_pid = RouterPid, translator_pid = TranslatorPid }, 
                Req, testing, FullUrl, RouterAdapter),
            From ! {self(), FullUrl, Result};
        Other ->
            error_logger:error_msg("Unexpected message in get_request_loop: ~p~n", [Other])
    end.

post_request_loop(AppInfo) ->
    put(boss_environment, testing),
    receive
        {From, Uri, Headers, Body} ->
            erlang:put(mochiweb_request_body, Body),
            erlang:put(mochiweb_request_body_length, length(Body)),
            erlang:put(mochiweb_request_post, mochiweb_util:parse_qs(Body)),
            [{_, RouterPid, _, _}]	= supervisor:which_children(AppInfo#boss_app_info.router_sup_pid),
            [{_, TranslatorPid, _, _}]	= supervisor:which_children(AppInfo#boss_app_info.translator_sup_pid),
			RouterAdapter = boss_env:router_adapter(),
            Req = make_request('POST', Uri, 
			       [{"Content-Encoding", "application/x-www-form-urlencoded"} | Headers]),
            FullUrl = Req:path(),
            Result = boss_web_controller_handle_request:process_request(AppInfo#boss_app_info{
							   router_pid     = RouterPid, 
							   translator_pid = TranslatorPid }, 
							 Req, 
							 testing, 
							 FullUrl,
							 RouterAdapter),

            From ! {self(), FullUrl, Result};
        Other ->
            error_logger:error_msg("Unexpected message in post_request_loop: ~p~n", [Other])
    end.

make_request(Method, Uri, Headers) ->
    Req = mochiweb_request:new(
        false, %Socket
        Method, Uri, {1, 0}, mochiweb_headers:make(Headers)),
    simple_bridge:make_request(mochiweb_request_bridge, Req).

receive_response(RequesterPid, Assertions, Continuations) ->
    {PushFun, PopFun} = make_stack_ops(),
    receive
        {RequesterPid, Uri, {Status, ResponseHeaders, ResponseBody}} ->
            receive_response_body(RequesterPid, Assertions, Continuations,
                                  PushFun, PopFun, Uri, Status, ResponseHeaders,
                                  ResponseBody);
        {'EXIT', _From, normal} ->
            receive_response(RequesterPid, Assertions, Continuations);
        Other ->
            error_logger:error_msg("Unexpected message in receive_response: ~p~n", [Other]),
            receive_response(RequesterPid, Assertions, Continuations)
    end.

receive_response_body(RequesterPid, Assertions, Continuations, PushFun,
                      PopFun, Uri, Status, ResponseHeaders, ResponseBody) ->
    ParsedResponseBody	= case ResponseBody of
			      []    -> [];
			      Other -> parse(ResponseHeaders, Other)
                         end,
    exit(RequesterPid, kill),
    ParsedResponse	= {Status, Uri, ResponseHeaders, ParsedResponseBody},
    boss_test:process_assertions_and_continuations(Assertions, Continuations, ParsedResponse,
        PushFun, PopFun, fun boss_db:dump/0).

make_stack_ops() ->
    PushFun = fun() ->
		      boss_db:push(),
		      boss_mail_driver_mock:push()
	      end,
    PopFun = fun() ->
		     boss_db:pop(),
		     boss_mail_driver_mock:pop()
             end,
    {PushFun, PopFun}.

parse([], Body) ->
    mochiweb_html:parse([<<"<html>">>, Body, <<"</html>">>]);
parse([Head|Tail], Body) ->
    case Head of
        {"Content-Type", "application/json"} -> mochijson2:decode(Body);
        _ -> parse(Tail, Body)
    end.
