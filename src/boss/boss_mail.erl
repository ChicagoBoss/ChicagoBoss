-module(boss_mail).
-export([start/1, stop/0, send/2, send/4, send/5]).

start(Options) ->
    boss_mail_sup:start_link(Options).

stop() ->
    ok.

send(Action, Args) ->
    boss_load:load_mail_controllers(),
    Result = apply(mail_controller, Action, Args),
    case Result of
        {ok, FromAddress, ToAddress, HeaderFields} ->
            send_message(FromAddress, ToAddress, Action, HeaderFields, []);
        {ok, FromAddress, ToAddress, HeaderFields, Variables} -> 
            send_message(FromAddress, ToAddress, Action, HeaderFields, Variables);
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

send_message(FromAddress, ToAddress, Action, HeaderFields, Variables) ->
    BodyFun = fun() -> build_message(Action, HeaderFields, Variables) end,
    gen_server:call(boss_mail, {deliver, FromAddress, ToAddress, BodyFun}).

build_message(Action, HeaderFields, Variables) ->
    ContentLanguage = proplists:get_value("Content-Language", HeaderFields),
    {MimeType, MessageBody} = build_message_body(Action, Variables, ContentLanguage),
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

build_message_body(Action, Variables, ContentLanguage) ->
    HtmlResult = render_view({Action, "html"}, Variables, ContentLanguage),
    TextResult = render_view({Action, "txt"}, Variables, ContentLanguage),
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

render_view({Action, Extension}, Variables, ContentLanguage) ->
    ViewPath = boss_files:mail_view_path(Action, Extension),
    case filelib:is_file(ViewPath) of
        true ->
            {ok, ViewModule} = boss_load:load_view_if_dev(ViewPath),
            TranslationFun = boss_translator:fun_for(ContentLanguage),
            ViewModule:render(Variables, TranslationFun);
        _ ->
            undefined
    end.

render_multipart_view(Parts, Boundary) ->
    ["This is a message with multiple parts in MIME format.\r\n",
        render_multipart_view1(Parts, Boundary)].

render_multipart_view1([], Boundary) ->
    ["--", Boundary, "--"];
render_multipart_view1([{MimeType, Body}|Rest], Boundary) ->
    ["--", Boundary, "\r\n", "Content-Type: ", MimeType, "\r\n\r\n",
        Body, "\r\n", render_multipart_view1(Rest, Boundary)].
