% Assertion helper functions
% Four-tuples are an HTTP response {Status, Location, Headers, ParsedHtml}, 
% three-tuples are an Email {Headers, TextBody, ParsedHtml}
-module(boss_assert).
-export([
        http_ok/1, 
        http_partial_content/1,
        http_redirect/1, 
        http_not_modified/1,
        http_bad_request/1,
        http_not_found/1,
        link_with_text/2, 
        tag_with_text/3, 
        header/3, 
        location_header/2,
        content_language_header/2,
        content_type_header/2,
        from_header/2,
        email_has_text/1, 
        email_has_html/1,
        email_is_text_only/1, 
        email_is_html_only/1, 
        email_is_multipart/1,
        email_received/1,
        email_not_received/1]).


-type http_information_code()  :: 100|101|102.
-type http_success_code()      :: 200|201|202|204|206|207.
-type http_redirect_code()     :: 300|301|302|303|304|305|306|307|308.
-type http_client_error_code() :: 400|401|402|403|404|405|406|407|408|409|410|411|412|413|414|415|416|417|418|419.

-type http_non_offical_code()  :: 420|422|423|424|425|450|451.
-type http_server_error_code() :: 500..511.
-type http_status_code()       :: http_information_code()|http_success_code()|http_redirect_code()|http_client_error_code()| http_server_error_code()|http_non_offical_code().
-type http_status_result()     :: {boolean(), string()}.
-type maybe(X) :: X|undefined.

-spec http_ok({http_status_code(),_,_,_})              -> http_status_result().
-spec http_partial_content({http_status_code(),_,_,_}) -> http_status_result().
-spec http_redirect({http_status_code(),_,_,_})        -> http_status_result().
-spec http_not_modified({http_status_code(),_,_,_})    -> http_status_result().
-spec http_bad_request({http_status_code(),_,_,_})     -> http_status_result().
-spec http_not_found({http_status_code(),_,_,_})       -> http_status_result().
-spec link_with_text([any()],{_,_,_} | {_,_,_,_}) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec tag_with_text([any()],[any()],{_,_,maybe_improper_list() | tuple()} | {_,_,_,maybe_improper_list() | tuple()}) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec header([any()],[any()],{[any()],_,_} | {_,_,[any()],_}) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec location_header([any()],{[any()],_,_} | {_,_,[any()],_}) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec content_language_header([any()],{[any()],_,_} | {_,_,[any()],_}) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec content_type_header([any()],{[any()],_,_} | {_,_,[any()],_}) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec from_header([any()],{[any()],_,_} | {_,_,[any()],_}) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec email_has_text({_,string(),_}) -> http_status_result().
-spec email_has_html({_,string(),_}) -> http_status_result().
-spec email_is_text_only({_,string(), string()}) -> http_status_result().
-spec email_is_html_only({_,string(), string()}) -> http_status_result().
-spec email_is_multipart({_,string(), string()}) -> http_status_result().
-spec email_received(maybe(string()))           -> http_status_result().
-spec email_not_received(maybe(string()))       -> http_status_result().
-spec tag_with_text1([any()],[any()],maybe_improper_list() | tuple()) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec link_with_text1([any()],_) -> {'false',[any(),...]} | {'true',[any(),...]}.
-spec has_tag_with_text(_,_,maybe_improper_list() | tuple()) -> boolean().
%% @spec http_ok(Response) -> {Passed, ErrorMessage}
%% @doc Compares the HTTP status code in `Response' to 200 (HTTP OK).
http_ok({Status, _, _, _} = _Response) ->
    {Status =:= 200, "HTTP Status not 200 OK"}.

%% @spec http_partial_content(Response) -> {Passed, ErrorMessage}
%% @doc Compares the HTTP status code in `Response' to 206 (HTTP Partial Content)
http_partial_content({Status, _, _, _} = _Response) ->
    {Status =:= 206, "HTTP Status not 206 Partial Content"}.

%% @spec http_redirect(Response) -> {Passed, ErrorMessage}
%% @doc Compares the HTTP status code in `Response' to 302 (HTTP Found).
http_redirect({Status, _, _, _} = _Response) ->
    {Status =:= 302, "HTTP Status not 302 Redirect"}.

%% @spec http_not_modified(Response) -> {Passed, ErrorMessage}
%% @doc Compares the HTTP status code in `Response' to 304 (HTTP Not Modified).
http_not_modified({Status, _, _, _} = _Response) ->
    {Status =:= 304, "HTTP Status not 304 Not Modified"}.

%% @spec http_bad_request(Response) -> {Passed, ErrorMessage}
%% @doc Compares the HTTP status code in `Response' to 400 (HTTP Bad Request).
http_bad_request({Status, _, _, _} = _Response) ->
    {Status =:= 400, "HTTP Status not 400 Bad Request"}.

%% @spec http_not_found(Response) -> {Passed, ErrorMessage}
%% @doc Compares the HTTP status code in `Response' to 404 (HTTP Not Found).
http_not_found({Status, _, _, _} = _Response) ->
    {Status =:= 404, "HTTP Status not 404 Not Found"}.

%% @spec link_with_text(Text, Response) -> {Passed, ErrorMessage}
%% @doc Looks in `Response' for a link with text equal to `Text'. 
%% The text may be the inner text of an &amp;lt;a&amp;gt; tag, or the 
%% "alt" attribute of a hyperlinked &amp;lt;img&amp;gt; tag.
%% `Response' may be an HTTP response, or an email.
link_with_text(Text, {_, _, _, ParseTree} = _Response) ->
    link_with_text1(Text, ParseTree);
link_with_text(Text, {_, _, ParseTree} = _Response) ->
    link_with_text1(Text, ParseTree).

%% @spec tag_with_text(Tag, Text, Response) -> {Passed, ErrorMessage}
%% @doc Looks in `Response' for an HTML tag of type `Tag' with inner
%% text equal to `Text'. `Response' may be an HTTP response, or an email.
tag_with_text(Tag, Text, {_, _, _, ParseTree} = _Response) ->
    tag_with_text1(Tag, Text, ParseTree);
tag_with_text(Tag, Text, {_, _, ParseTree} = _Response) ->
    tag_with_text1(Tag, Text, ParseTree).

%% @spec header(Key, Value, Response) -> {Passed, ErrorMessage}
%% @doc Compares the `Key' header in `Response' (HTTP or email) to `Value'.
header(Key, Value, {_, _, Headers, _} = _Response) ->
    {proplists:get_value(Key, Headers) =:= Value,
        "\""++Key++"\" header is not equal to \""++Value++"\""};
header(Key, Value, {Headers, _, _} = _Response) ->
    {proplists:get_value(list_to_binary(Key), Headers) =:= list_to_binary(Value),
        "\""++Key++"\" header is not equal to \""++Value++"\""}.

%% @spec location_header(Url, Response) -> {Passed, ErrorMessage}
%% @doc Compares `Url' to the Location: header of `Response'.
location_header(Url, Response) ->
    header("Location", Url, Response).

%% @spec content_language_header(ContentLanguage, Response) -> {Passed, ErrorMessage}
%% @doc Compares `ContentLanguage' to the Content-Language: header of `Response'
content_language_header(ContentLanguage, Response) ->
    header("Content-Language", ContentLanguage, Response).

%% @spec content_type_header(ContentType, Response) -> {Passed, ErrorMessage}
%% @doc Compares `ContentType' to the Content-Type: header of `Response'
content_type_header(ContentType, Response) ->
    header("Content-Type", ContentType, Response).

%% @spec from_header(FromAddress, Email) -> {Passed, ErrorMessage}
%% @doc Compares `FromAddress' to the From: header field in `Email'
from_header(FromAddress, Email) ->
    header("From", FromAddress, Email).

%% @spec email_has_text(Email) -> {Passed, ErrorMessage}
%% @doc Checks whether `Email' contains a plain-text body.
email_has_text({_, TextBody, _} = _Email) ->
    {TextBody =/= "", "Email does not contain a plain-text body"}.

%% @spec email_has_html(Email) -> {Passed, ErrorMessage}
%% @doc Checks whether `Email' contains an HTML body.
email_has_html({_, TextBody, _} = _Email) ->
    {TextBody =/= "", "Email does not contain an HTML body"}.

%% @spec email_is_text_only(Email) -> {Passed, ErrorMessage}
%% @doc Checks whether `Email' contains a plain-text body and not an HTML body.
email_is_text_only({_, TextBody, HtmlBody} = _Email) ->
    {TextBody =/= "" andalso HtmlBody =:= "",
        "Email is not text-only"}.

%% @spec email_is_html_only(Email) -> {Passed, ErrorMessage}
%% @doc Checks whether `Email' contains an HTML body and not a plain-text body.
email_is_html_only({_, TextBody, HtmlBody} = _Email) ->
    {TextBody =:= "" andalso HtmlBody =/= "",
        "Email is not HTML-only"}.

%% @spec email_is_multipart(Email) -> {Passed, ErrorMessage}
%% @doc Checks whether `Email' contains alternative text and HTML representations.
email_is_multipart({_, TextBody, HtmlBody} = _Email) ->
    {TextBody =/= "" andalso HtmlBody =/= "",
        "Email is not multipart"}.

%% @spec email_received(Email) -> {Passed, ErrorMessage}
%% @doc Checks whether `Email' exists
email_received(Email) ->
    {Email =/= undefined, "Email was not received"}.

%% @spec email_not_received(Email) -> {Passed, ErrorMessage}
%% @doc Checks whether `Email' is undefined
email_not_received(Email) ->
    {Email =:= undefined, "Email was not supposed to be received (but was)"}.


% internal

tag_with_text1(Tag, Text, ParseTree) ->
    {has_tag_with_text(Tag, Text, ParseTree), "No <"++Tag++"> tag containing \""++Text++"\""}.

link_with_text1(Text, Response) ->
    {boss_web_test:find_link_with_text(Text, Response) =/= undefined,
        "No link to \""++Text++"\""}.

has_tag_with_text(Tag, Text, [{comment, _}|Rest]) ->
    has_tag_with_text(Tag, Text, Rest);
has_tag_with_text(Tag, Text, ParseTree) when is_list(Tag) ->
    has_tag_with_text(list_to_binary(Tag), Text, ParseTree);
has_tag_with_text(Tag, Text, ParseTree) when is_list(Text) ->
    has_tag_with_text(Tag, list_to_binary(Text), ParseTree);
has_tag_with_text(Tag, Text, ParseTree) when is_tuple(ParseTree) ->
    has_tag_with_text(Tag, Text, [ParseTree]);
has_tag_with_text(_, _, []) ->
    false;
has_tag_with_text(Tag, Text, [SomeText|Rest]) when is_binary(SomeText) ->
    has_tag_with_text(Tag, Text, Rest);
has_tag_with_text(Tag, Text, [{Tag, _, [Text]}|_Rest]) ->
    true;
has_tag_with_text(Tag, Text, [{_OtherTag, _, Children}|Rest]) ->
    has_tag_with_text(Tag, Text, Children) orelse has_tag_with_text(Tag, Text, Rest).
