-module(boss_mail).
-export([start/1, stop/0, send_template/3, send_template/4, 
        send/4, send/5, send/6]).

-spec start(_) -> any().
-spec stop() -> 'ok'.
-spec send_template(types:application(),atom(),[any()]) -> any().
-spec send_template(types:application(),atom(),[any()],_) -> any().
-spec send(_,_,_,_) -> any().
-spec send(_,_,_,atom() | binary() | string(),[any()]) -> any().
-spec send(_,_,_,atom() | binary() | string(),[any()],_) -> any().
-spec do_send(_,_,_,_,_) -> any().
-spec send_message(_,_,_,atom(),_,_,_,_) -> any().
-spec build_message(_,atom(),[{_,_}],_,[any()]) -> [[any()],...].
-spec convert_unix_newlines_to_dos(binary() | [any()]) -> [any()].
-spec convert_unix_newlines_to_dos([any()],[any()]) -> [any()].
-spec build_message_header([{_,_}],[[[any()] | 61 | 95] | 1..255,...],_) -> [[any(),...]].
-spec add_fields([{_,_}],[any()],[[any(),...]]) -> [[any(),...]].
-spec build_message_body_attachments(_,_,_,[{_,_} | {_,_,binary() | maybe_improper_list(any(),binary() | [])}],_,_) -> 'undefined' | {[[[any()] | 61 | 95] | 1..255,...],_}.
-spec build_message_body(_,_,_,_,_) -> 'undefined' | {[[[any()] | 61 | 95] | 1..255,...],_}.
-spec render_view(types:application(),{_,[104 | 108 | 109 | 116 | 120,...]},_,_) -> any().
-spec render_multipart_view([{_,_} | {_,_,binary() | maybe_improper_list(any(),binary() | [])},...],[[[any()] | 61 | 95],...],_) -> [[any(),...],...].
-spec render_multipart_view1([{_,_} | {_,_,binary() | maybe_improper_list(any(),binary() | [])}],[[[any()] | 61 | 95],...],_) -> [any(),...].
-spec wrap_to_76(binary()) -> [binary(),...].
-spec wrap_to_76(binary(),[<<_:16,_:_*592>>]) -> binary().

start(Options) ->
    boss_mail_sup:start_link(Options).

stop() ->
    ok.

send_template(Application, Action, Args) ->
    send_template(Application, Action, Args, undefined).

send_template(Application, Action, Args, Callback) ->
    boss_load:load_mail_controllers(Application),
    Controller = list_to_atom(lists:concat([Application, "_outgoing_mail_controller"])),
    case apply(Controller, Action, Args) of
        {ok, FromAddress, ToAddress, HeaderFields} ->
            send_message(Application, FromAddress, ToAddress, Action, HeaderFields, [], [], Callback);
        {ok, FromAddress, ToAddress, HeaderFields, Variables} -> 
            send_message(Application, FromAddress, ToAddress, Action, HeaderFields, Variables, [], Callback);
        {ok, FromAddress, ToAddress, HeaderFields, Variables, Options} -> 
            send_message(Application, FromAddress, ToAddress, Action, HeaderFields, Variables, Options, Callback);
        {nevermind, Reason} ->
            lager:info("Mail Not sent because of ~p", [Reason]),
	    {ok, Reason};
        nevermind ->
            lager:info("Mail Not sent no reason"),
            ok
    end.

send(FromAddress, ToAddress, Subject, Body) ->
    do_send(FromAddress, ToAddress, Subject, Body, undefined).

send(FromAddress, ToAddress, Subject, Body, BodyArgs) ->
    send(FromAddress, ToAddress, Subject, Body, BodyArgs, undefined).

send(FromAddress, ToAddress, Subject, Body, BodyArgs, Callback) ->
    do_send(FromAddress, ToAddress, Subject, io_lib:format(Body, BodyArgs), Callback).

do_send(FromAddress, ToAddress, Subject, Body, Callback) ->
    MessageHeader = build_message_header([
                                          {"Subject", Subject},
                                          {"To", ToAddress},
                                          {"From", FromAddress}],
                                         "text/plain",
                                         undefined),
    gen_server:call(boss_mail, {deliver, FromAddress, ToAddress, 
            fun() -> [MessageHeader, "\r\n", convert_unix_newlines_to_dos(Body)] end,
            Callback}).

send_message(App, FromAddress, ToAddress, Action, HeaderFields, Variables, Options, Callback) ->
    BodyFun = fun() -> build_message(App, Action, HeaderFields, Variables, Options) end,
    gen_server:call(boss_mail, {deliver, FromAddress, ToAddress, BodyFun, Callback}).

build_message(App, Action, HeaderFields, Variables, Options) ->
    ContentLanguage             = proplists:get_value("Content-Language", HeaderFields),
    EffectiveAction             = proplists:get_value(template, Options, Action),
    Attachments                 = proplists:get_value(attachments, Options, []),
    CharSet                     = proplists:get_value(charset, Options),
    {MimeType, MessageBody}     = build_message_body_attachments(App, EffectiveAction, Variables, Attachments, ContentLanguage, CharSet),
    MessageHeader = build_message_header(HeaderFields, MimeType, CharSet),
    [MessageHeader, "\r\n", convert_unix_newlines_to_dos(MessageBody)].

convert_unix_newlines_to_dos(Body) when is_binary(Body) ->
    convert_unix_newlines_to_dos(binary_to_list(Body));
convert_unix_newlines_to_dos(Body) when is_list(Body) ->
    convert_unix_newlines_to_dos(Body, []).

convert_unix_newlines_to_dos([], Acc) ->
    lists:reverse(Acc);
convert_unix_newlines_to_dos([$\r, $\n|Rest], Acc) ->
    convert_unix_newlines_to_dos(Rest, [$\n, $\r|Acc]);
convert_unix_newlines_to_dos([$\n|Rest], Acc) ->
    convert_unix_newlines_to_dos(Rest, [$\n, $\r|Acc]);
convert_unix_newlines_to_dos([H|T], Acc) when is_binary(H); is_list(H) ->
    convert_unix_newlines_to_dos(T, [convert_unix_newlines_to_dos(H)|Acc]);
convert_unix_newlines_to_dos([H|T], Acc) ->
    convert_unix_newlines_to_dos(T, [H|Acc]).

build_content_type(ContentType, CharSet) ->
    case CharSet of
        undefined ->
            ContentType;
        _ ->
            io_lib:format("~s; charset=~s", [ContentType, CharSet])
    end.

build_message_header(HeaderFields, DefaultMimeType, CharSet) ->
    MessageID = case proplists:get_value("Message-ID", HeaderFields) of
        undefined -> smtp_util:generate_message_id();
        Other -> Other
    end,
    ContentType = build_content_type(proplists:get_value("Content-Type", HeaderFields, DefaultMimeType), CharSet),
    Date = proplists:get_value("Date", HeaderFields, erlydtl_dateformat:format("r")),
    AllHeaders = [{"Date", Date}, {"Content-Type", ContentType},
        {"MIME-Version", "1.0"}, {"Message-ID", MessageID} | HeaderFields],
    add_fields(AllHeaders, [], []).

add_fields([], _, Acc) ->
    lists:reverse(Acc);
add_fields([{Key, Value}|Rest], Seen, Acc) ->
    case proplists:get_value(Key, Seen) of
        undefined ->
            add_fields(Rest, [Key|Seen], [[Key, ": ", Value, "\r\n"] | Acc]);
        _ ->
            add_fields(Rest, Seen, Acc)
    end.

build_message_body_attachments(App, Action, Variables, [], ContentLanguage, CharSet) ->
    build_message_body(App, Action, Variables, ContentLanguage, CharSet);
build_message_body_attachments(App, Action, Variables, Attachments, ContentLanguage, CharSet) ->
    Boundary = smtp_util:generate_message_boundary(),
    {MimeType, MessageBody} = build_message_body(App, Action, Variables, ContentLanguage, CharSet),
    {"multipart/mixed; boundary=\""++Boundary++"\"",
        render_multipart_view([{MimeType, MessageBody}|Attachments], Boundary, CharSet)}.

build_message_body(App, Action, Variables, ContentLanguage, CharSet) ->
    HtmlResult = render_view(App, {Action, "html"}, Variables, ContentLanguage),
    TextResult = render_view(App, {Action, "txt"}, Variables, ContentLanguage),
    case HtmlResult of
        undefined ->
            case TextResult of
                undefined -> undefined;
                {ok, TextView} ->
                    {"text/plain", TextView}
            end;
        {ok, HtmlView} ->
            case TextResult of
                undefined -> {"text/html", HtmlView};
                {ok, TextView} ->
                    Boundary = smtp_util:generate_message_boundary(),
                    {"multipart/alternative; boundary=\""++Boundary++"\"",
                        render_multipart_view(
                            [{"text/plain", TextView}, 
                                {"text/html", HtmlView}], Boundary, CharSet)}
            end
    end.

render_view(App, {Action, Extension}, Variables, ContentLanguage) ->
    ViewPath = boss_files_util:mail_view_path(Action, Extension),
    ViewModules = boss_files:view_module_list(App),
    TranslatorPid = boss_web:translator_pid(App),
    RouterPid = boss_web:router_pid(App),
    case boss_load:load_view_if_dev(App, ViewPath, ViewModules, TranslatorPid) of
        {ok, ViewModule, _ViewAdapter} ->
            TranslationFun = boss_translator:fun_for(TranslatorPid, ContentLanguage),
            ViewModule:render([{"_lang", ContentLanguage}|Variables], 
                [{translation_fun, TranslationFun}, {locale, ContentLanguage}, 
                    {application, App}, {router_pid, RouterPid}]);
        _ ->
            undefined
    end.

render_multipart_view(Parts, Boundary, CharSet) ->
    ["This is a message with multiple parts in MIME format.\r\n",
        render_multipart_view1(Parts, Boundary, CharSet)].

render_multipart_view1([], Boundary, _) ->
    ["--", Boundary, "--"];
render_multipart_view1([{FileName, MimeType, Body}|Rest], Boundary, CharSet) ->
    ["--", Boundary, 
        "\r\n", "Content-Type: ", build_content_type(MimeType, CharSet), 
        "\r\n", "Content-Disposition: attachment; filename=", FileName, 
        "\r\n", "Content-Transfer-Encoding: base64",
        "\r\n\r\n",
        wrap_to_76(base64:encode(erlang:iolist_to_binary(Body))), "\r\n", render_multipart_view1(Rest, Boundary, CharSet)];
render_multipart_view1([{MimeType, Body}|Rest], Boundary, CharSet) ->
    ["--", Boundary, "\r\n", "Content-Type: ", build_content_type(MimeType, CharSet), "\r\n", "Content-Transfer-Encoding: base64", "\r\n\r\n",
        wrap_to_76(base64:encode(erlang:iolist_to_binary(Body))), "\r\n", render_multipart_view1(Rest, Boundary, CharSet)].

wrap_to_76(String) ->
    [wrap_to_76(String, [])].

wrap_to_76(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
wrap_to_76(<<Head:76/binary, Tail/binary>>, Acc) ->
    wrap_to_76(Tail, [<<"\r\n">>, Head | Acc]);
wrap_to_76(Head, Acc) ->
    list_to_binary(lists:reverse([<<"\r\n">>, Head | Acc])).

