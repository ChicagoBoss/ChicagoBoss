-module(boss_mail).
-export([start/1, stop/0, send_template/3, send/4, send/5]).

start(Options) ->
    boss_mail_sup:start_link(Options).

stop() ->
    ok.

send_template(Application, Action, Args) ->
    boss_load:load_mail_controllers(),
    Controller = list_to_atom(lists:concat([Application, "_outgoing_mail_controller"])),
    case apply(Controller, Action, Args) of
        {MailAction, FromAddress, ToAddress, HeaderFields} ->
            send_message(Application, FromAddress, ToAddress, Action, HeaderFields, [], [], MailAction);
        {MailAction, FromAddress, ToAddress, HeaderFields, Variables} -> 
            send_message(Application, FromAddress, ToAddress, Action, HeaderFields, Variables, [], MailAction);
        {MailAction, FromAddress, ToAddress, HeaderFields, Variables, Attachments} -> 
            send_message(Application, FromAddress, ToAddress, Action, HeaderFields, Variables, Attachments, MailAction);
        nevermind ->
            ok
    end.

send(FromAddress, ToAddress, Subject, Body, BodyArgs) ->
    send(FromAddress, ToAddress, Subject, io_lib:format(Body, BodyArgs)).

send(FromAddress, ToAddress, Subject, Body) ->
    MessageHeader = build_message_header([
            {"Subject", Subject},
            {"To", ToAddress},
            {"From", FromAddress}], "text/plain"), 
    gen_server:call(boss_mail, {deliver, FromAddress, ToAddress, fun() -> [MessageHeader, "\r\n", Body] end}).

send_message(App, FromAddress, ToAddress, Action, HeaderFields, Variables, Attachments, MailAction) ->
    BodyFun = fun() -> build_message(App, Action, HeaderFields, Variables, Attachments, MailAction) end,
    gen_server:call(boss_mail, {deliver, FromAddress, ToAddress, BodyFun}).

build_message(App, Action, HeaderFields, Variables, Attachments, MailAction) ->
    ContentLanguage = proplists:get_value("Content-Language", HeaderFields),
    {MimeType, MessageBody} = build_message_body_attachments(App, Action, Variables, Attachments, ContentLanguage, MailAction),
    MessageHeader = build_message_header(HeaderFields, MimeType),
    [MessageHeader, "\r\n", MessageBody].

build_message_header(HeaderFields, DefaultMimeType) ->
    MessageID = case proplists:get_value("Message-ID", HeaderFields) of
        undefined -> smtp_util:generate_message_id();
        Other -> Other
    end,
    ContentType = proplists:get_value("Content-Type", HeaderFields, DefaultMimeType),
    Date = proplists:get_value("Date", HeaderFields, erlydtl_dateformat:format("r")),
    BaseHeader = ["Date: ", Date, "\r\n",
        "Content-Type: ", ContentType, "\r\n",
        "MIME-Version: ", "1.0", "\r\n",
        "Message-ID: ", MessageID, "\r\n"],
    add_fields(["Subject", "From", "To", "Reply-To"], HeaderFields, BaseHeader).

add_fields([], _, Acc) ->
    Acc;
add_fields([Field|Rest], HeaderFields, Acc) ->
    case proplists:get_value(Field, HeaderFields) of
        undefined ->
            add_fields(Rest, HeaderFields, Acc);
        Value ->
            add_fields(Rest, HeaderFields, [Field, ": ", Value, "\r\n" | Acc])
    end.

build_message_body_attachments(App, Action, Variables, [], ContentLanguage, MailAction) ->
    build_message_body(App, Action, Variables, ContentLanguage, MailAction);
build_message_body_attachments(App, Action, Variables, Attachments, ContentLanguage, MailAction) ->
    Boundary = smtp_util:generate_message_boundary(),
    {MimeType, MessageBody} = build_message_body(App, Action, Variables, ContentLanguage, MailAction),
    {"multipart/mixed; boundary=\""++Boundary++"\"",
        render_multipart_view([{MimeType, MessageBody}|Attachments], Boundary)}.

build_message_body(App, Action, Variables, ContentLanguage, MailAction) ->
	case MailAction of
		ok ->
    			HtmlResult = render_view(App, {Action, "html"}, Variables, ContentLanguage),
    			TextResult = render_view(App, {Action, "txt"}, Variables, ContentLanguage);
    		{render_other, TemplName} ->
    			HtmlResult = render_view(App, {TemplName, "html"}, Variables, ContentLanguage),
    			TextResult = render_view(App, {TemplName, "txt"}, Variables, ContentLanguage)
    	end,
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
                                {"text/html", HtmlView}], Boundary)}
            end
    end.

render_view(App, {Action, Extension}, Variables, ContentLanguage) ->
    ViewPath = boss_files:mail_view_path(Action, Extension),
    ViewModule = boss_load:view_module(App, ViewPath),
    TranslatorPid = boss_web:translator_pid(App),
    case filelib:is_file(ViewPath) orelse code:is_loaded(ViewModule) =/= false of
        true ->
            {ok, _} = boss_load:load_view_if_dev(App, ViewPath, TranslatorPid),
            TranslationFun = boss_translator:fun_for(TranslatorPid, ContentLanguage),
            ViewModule:render([{"_lang", ContentLanguage}|Variables], [{translation_fun, TranslationFun}, {locale, ContentLanguage}]);
        _ ->
            undefined
    end.

render_multipart_view(Parts, Boundary) ->
    ["This is a message with multiple parts in MIME format.\r\n",
        render_multipart_view1(Parts, Boundary)].

render_multipart_view1([], Boundary) ->
    ["--", Boundary, "--"];
render_multipart_view1([{FileName, MimeType, Body}|Rest], Boundary) ->
    ["--", Boundary, 
        "\r\n", "Content-Type: ", MimeType, 
        "\r\n", "Content-Disposition: attachment; filename=", FileName, 
        "\r\n", "Content-Transfer-Encoding: base64",
        "\r\n\r\n",
        wrap_to_76(base64:encode(erlang:iolist_to_binary(Body))), "\r\n", render_multipart_view1(Rest, Boundary)];
render_multipart_view1([{MimeType, Body}|Rest], Boundary) ->
    ["--", Boundary, "\r\n", "Content-Type: ", MimeType, "\r\n\r\n",
        Body, "\r\n", render_multipart_view1(Rest, Boundary)].


wrap_to_76(String) ->
    [wrap_to_76(String, [])].

wrap_to_76(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
wrap_to_76(<<Head:76/binary, Tail/binary>>, Acc) ->
    wrap_to_76(Tail, [<<"\r\n">>, Head | Acc]);
wrap_to_76(Head, Acc) ->
    list_to_binary(lists:reverse([<<"\r\n">>, Head | Acc])).

