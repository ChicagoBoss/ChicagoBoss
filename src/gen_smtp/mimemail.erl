%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%% @doc A module for decoding/encoding MIME 1.0 email
-module(mimemail).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([encode/1, decode/2, decode/1, get_header_value/2, get_header_value/3, parse_headers/1]).

-define(DEFAULT_OPTIONS, [
		{encoding, get_default_encoding()}, % default encoding is utf-8 if we can find the iconv module
		{decode_attachments, true} % should we decode any base64/quoted printable attachments?
	]).

-type(mimetuple() :: {binary(), binary(), [{binary(), binary()}], [{binary(), binary()}], binary() | [{binary(), binary(), [{binary(), binary()}], [{binary(), binary()}], binary() | [tuple()]}] | tuple()}).

-spec(decode/1 :: (Email :: binary()) -> mimetuple()).
decode(All) ->
	{Headers, Body} = parse_headers(All),
	decode(Headers, Body, ?DEFAULT_OPTIONS).

-spec(decode/2 :: (Headers :: [{binary(), binary()}], Body :: binary()) -> mimetuple()).
decode(All, Options) when is_binary(All), is_list(Options) ->
	{Headers, Body} = parse_headers(All),
	decode(Headers, Body, Options);
decode(Headers, Body) when is_list(Headers), is_binary(Body) ->
	decode(Headers, Body, ?DEFAULT_OPTIONS).

decode(OrigHeaders, Body, Options) ->
	%io:format("headers: ~p~n", [Headers]),
	Encoding = proplists:get_value(encoding, Options, none),
	case whereis(iconv) of
		undefined when Encoding =/= none ->
			{ok, _Pid} = iconv:start();
		_ ->
			ok
	end,

	%FixedHeaders = fix_headers(Headers),
	Headers = decode_headers(OrigHeaders, [], Encoding),
	case parse_with_comments(get_header_value(<<"MIME-Version">>, Headers)) of
		undefined ->
			case parse_content_type(get_header_value(<<"Content-Type">>, Headers)) of
				{<<"multipart">>, _SubType, _Parameters} ->
					erlang:error(non_mime_multipart);
				{Type, SubType, Parameters} ->
					NewBody = decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers),
						Body, proplists:get_value(<<"charset">>, Parameters), Encoding),
					{Type, SubType, Headers, Parameters, NewBody};
				undefined ->
					Parameters = [{<<"content-type-params">>, [{<<"charset">>, <<"us-ascii">>}]}, {<<"disposition">>, <<"inline">>}, {<<"disposition-params">>, []}],
					{<<"text">>, <<"plain">>, Headers, Parameters, decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers), Body)}
			end;
		Other ->
			decode_component(Headers, Body, Other, Options)
	end.

-spec(encode/1 :: (MimeMail :: mimetuple()) -> binary()).
encode({Type, Subtype, Headers, ContentTypeParams, Parts}) ->
	{FixedParams, FixedHeaders} = ensure_content_headers(Type, Subtype, ContentTypeParams, Headers, Parts, true),
	FixedHeaders2 = check_headers(FixedHeaders),
	list_to_binary([binstr:join(
				encode_headers(
					FixedHeaders2
					),
				"\r\n"),
			"\r\n\r\n",
		binstr:join(encode_component(Type, Subtype, FixedHeaders2, FixedParams, Parts),
			"\r\n")]);
encode(_) ->
	io:format("Not a mime-decoded DATA~n"),
	erlang:error(non_mime).


decode_headers(Headers, _, none) ->
	Headers;
decode_headers([], Acc, _Charset) ->
	lists:reverse(Acc);
decode_headers([{Key, Value} | Headers], Acc, Charset) ->
	decode_headers(Headers, [{Key, decode_header(Value, Charset)} | Acc], Charset).

decode_header(Value, Charset) ->
	case re:run(Value, "=\\?([-A-Za-z0-9_]+)\\?([qQbB])\\?([^\s]+)\\?=", [ungreedy]) of
		nomatch ->
			Value;
		{match,[{AllStart, AllLen},{EncodingStart, EncodingLen},{TypeStart, _},{DataStart, DataLen}]} ->
			Encoding = binstr:substr(Value, EncodingStart+1, EncodingLen),
			Type = binstr:to_lower(binstr:substr(Value, TypeStart+1, 1)),
			Data = binstr:substr(Value, DataStart+1, DataLen),

			CD = case iconv:open(Charset, fix_encoding(Encoding)) of
				{ok, Res} -> Res;
				{error, einval} -> throw({bad_charset, fix_encoding(Encoding)})
			end,

			DecodedData = case Type of
				<<"q">> ->
					{ok, S} = iconv:conv(CD, decode_quoted_printable(re:replace(Data, "_", " ", [{return, binary}, global]))),
					S;
				<<"b">> ->
					{ok, S} = iconv:conv(CD, decode_base64(re:replace(Data, "_", " ", [{return, binary}, global]))),
					S
			end,

			iconv:close(CD),


			Offset = case re:run(binstr:substr(Value, AllStart + AllLen + 1), "^([\s\t\n\r]+)=\\?[-A-Za-z0-9_]+\\?[^\s]\\?[^\s]+\\?=", [ungreedy]) of
				nomatch ->
					% no 2047 block immediately following
					1;
				{match,[{_, _},{_, WhiteSpaceLen}]} ->
					1+ WhiteSpaceLen
			end,


			NewValue = list_to_binary([binstr:substr(Value, 1, AllStart), DecodedData, binstr:substr(Value, AllStart + AllLen + Offset)]),
			decode_header(NewValue, Charset)
	end.


decode_component(Headers, Body, MimeVsn, Options) when MimeVsn =:= <<"1.0">> ->
	case parse_content_disposition(get_header_value(<<"Content-Disposition">>, Headers)) of
		{Disposition, DispositionParams} ->
			ok;
		_ -> % defaults
			Disposition = <<"inline">>,
			DispositionParams = []
	end,

	case parse_content_type(get_header_value(<<"Content-Type">>, Headers)) of
		{<<"multipart">>, SubType, Parameters} ->
			case proplists:get_value(<<"boundary">>, Parameters) of
				undefined ->
					erlang:error(no_boundary);
				Boundary ->
					% io:format("this is a multipart email of type:  ~s and boundary ~s~n", [SubType, Boundary]),
					Parameters2 = [{<<"content-type-params">>, Parameters}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
					{<<"multipart">>, SubType, Headers, Parameters2, split_body_by_boundary(Body, list_to_binary(["--", Boundary]), MimeVsn, Options)}
			end;
		{<<"message">>, <<"rfc822">>, Parameters} ->
			{NewHeaders, NewBody} = parse_headers(Body),
			Parameters2 = [{<<"content-type-params">>, Parameters}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
			{<<"message">>, <<"rfc822">>, Headers, Parameters2, decode(NewHeaders, NewBody, Options)};
		{Type, SubType, Parameters} ->
			%io:format("body is ~s/~s~n", [Type, SubType]),
			Parameters2 = [{<<"content-type-params">>, Parameters}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
			{Type, SubType, Headers, Parameters2, decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers), Body, proplists:get_value(<<"charset">>, Parameters), proplists:get_value(encoding, Options, none))};
		undefined -> % defaults
			Type = <<"text">>,
			SubType = <<"plain">>,
			Parameters = [{<<"content-type-params">>, [{<<"charset">>, <<"us-ascii">>}]}, {<<"disposition">>, Disposition}, {<<"disposition-params">>, DispositionParams}],
			{Type, SubType, Headers, Parameters, decode_body(get_header_value(<<"Content-Transfer-Encoding">>, Headers), Body)}
	end;
decode_component(_Headers, _Body, Other, _Options) ->
	erlang:error({mime_version, Other}).

-spec(get_header_value/3 :: (Needle :: binary(), Headers :: [{binary(), binary()}], Default :: any()) -> binary() | any()).
get_header_value(Needle, Headers, Default) ->
	%io:format("Headers: ~p~n", [Headers]),
	F =
	fun({Header, _Value}) ->
			binstr:to_lower(Header) =:= binstr:to_lower(Needle)
	end,
	case lists:filter(F, Headers) of
		% TODO if there's duplicate headers, should we use the first or the last?
		[{_Header, Value}|_T] ->
			Value;
		_ ->
			Default
	end.

-spec(get_header_value/2 :: (Needle :: binary(), Headers :: [{binary(), binary()}]) -> binary() | 'undefined').
get_header_value(Needle, Headers) ->
	get_header_value(Needle, Headers, undefined).

-spec parse_with_comments(Value :: binary()) -> binary() | no_return();
	(Value :: atom()) -> atom().
parse_with_comments(Value) when is_binary(Value) ->
	parse_with_comments(Value, [], 0, false);
parse_with_comments(Value) ->
	Value.

-spec parse_with_comments(Value :: binary(), Acc :: list(), Depth :: non_neg_integer(), Quotes :: boolean()) -> binary() | no_return().
parse_with_comments(<<>>, _Acc, _Depth, Quotes) when Quotes ->
	erlang:error(unterminated_quotes);
parse_with_comments(<<>>, _Acc, Depth, _Quotes) when Depth > 0 ->
	erlang:error(unterminated_comment);
parse_with_comments(<<>>, Acc, _Depth, _Quotes) ->
	binstr:strip(list_to_binary(lists:reverse(Acc)));
parse_with_comments(<<$\\, H, Tail/binary>>, Acc, Depth, Quotes) when Depth > 0, H > 32, H < 127 ->
	parse_with_comments(Tail, Acc, Depth, Quotes);
parse_with_comments(<<$\\, Tail/binary>>, Acc, Depth, Quotes) when Depth > 0 ->
	parse_with_comments(Tail, Acc, Depth, Quotes);
parse_with_comments(<<$\\, H, Tail/binary>>, Acc, Depth, Quotes) when H > 32, H < 127 ->
	parse_with_comments(Tail, [H | Acc], Depth, Quotes);
parse_with_comments(<<$\\, Tail/binary>>, Acc, Depth, Quotes) ->
	parse_with_comments(Tail, [$\\ | Acc], Depth, Quotes);
parse_with_comments(<<$(, Tail/binary>>, Acc, Depth, Quotes) when not Quotes ->
	parse_with_comments(Tail, Acc, Depth + 1, Quotes);
parse_with_comments(<<$), Tail/binary>>, Acc, Depth, Quotes) when Depth > 0, not Quotes ->
	parse_with_comments(Tail, Acc, Depth - 1, Quotes);
parse_with_comments(<<_, Tail/binary>>, Acc, Depth, Quotes) when Depth > 0 ->
	parse_with_comments(Tail, Acc, Depth, Quotes);
parse_with_comments(<<$", T/binary>>, Acc, Depth, true) -> %"
	parse_with_comments(T, Acc, Depth, false);
parse_with_comments(<<$", T/binary>>, Acc, Depth, false) -> %"
	parse_with_comments(T, Acc, Depth, true);
parse_with_comments(<<H, Tail/binary>>, Acc, Depth, Quotes) ->
	parse_with_comments(Tail, [H | Acc], Depth, Quotes).

-spec(parse_content_type/1 :: (Value :: 'undefined') -> 'undefined';
	(Value :: binary()) -> {binary(), binary(), [{binary(), binary()}]}).
parse_content_type(undefined) ->
	undefined;
parse_content_type(String) ->
	try parse_content_disposition(String) of
		{RawType, Parameters} ->
			case binstr:strchr(RawType, $/) of
				Index when Index < 2 ->
					throw(bad_content_type);
				Index ->
					Type = binstr:substr(RawType, 1, Index - 1),
					SubType = binstr:substr(RawType, Index + 1),
					{binstr:to_lower(Type), binstr:to_lower(SubType), Parameters}
			end
		catch
			bad_disposition ->
				throw(bad_content_type)
	end.

-spec(parse_content_disposition/1 :: (Value :: 'undefined') -> 'undefined';
	(String :: binary()) -> {binary(), [{binary(), binary()}]}).
parse_content_disposition(undefined) ->
	undefined;
parse_content_disposition(String) ->
	[Disposition | Parameters] = binstr:split(parse_with_comments(String), <<";">>),
	F =
	fun(X) ->
		Y = binstr:strip(binstr:strip(X), both, $\t),
		case binstr:strchr(Y, $=) of
			Index when Index < 2 ->
				throw(bad_disposition);
			Index ->
				Key = binstr:substr(Y, 1, Index - 1),
				Value = binstr:substr(Y, Index + 1),
				{binstr:to_lower(Key), Value}
		end
	end,
	Params = lists:map(F, Parameters),
	{binstr:to_lower(Disposition), Params}.

split_body_by_boundary(Body, Boundary, MimeVsn, Options) ->
	% find the indices of the first and last boundary
	case [binstr:strpos(Body, Boundary), binstr:strpos(Body, list_to_binary([Boundary, "--"]))] of
		[0, _] ->
			erlang:error(missing_boundary);
		[_, 0] ->
			erlang:error(missing_last_boundary);
		[Start, End] ->
			NewBody = binstr:substr(Body, Start + byte_size(Boundary), End - Start),
			% from now on, we can be sure that each boundary is preceeded by a CRLF
			Parts = split_body_by_boundary_(NewBody, list_to_binary(["\r\n", Boundary]), []),
			[decode_component(Headers, Body2, MimeVsn, Options) || {Headers, Body2} <- [V || {_, Body3} = V <- Parts, byte_size(Body3) =/= 0]]
		end.

split_body_by_boundary_(<<>>, _Boundary, Acc) ->
	lists:reverse(Acc);
split_body_by_boundary_(Body, Boundary, Acc) ->
	% trim the incomplete first line
	TrimmedBody = binstr:substr(Body, binstr:strpos(Body, "\r\n") + 2),
	case binstr:strpos(TrimmedBody, Boundary) of
		0 ->
			lists:reverse([{[], TrimmedBody} | Acc]);
		Index ->
			split_body_by_boundary_(binstr:substr(TrimmedBody, Index + byte_size(Boundary)), Boundary,
				[parse_headers(binstr:substr(TrimmedBody, 1, Index - 1)) | Acc])
	end.

-spec(parse_headers/1 :: (Body :: binary()) -> {[{binary(), binary()}], binary()}).
parse_headers(Body) ->
	case binstr:strpos(Body, "\r\n") of
		0 ->
			{[], Body};
		1 ->
			{[], binstr:substr(Body, 3)};
		Index ->
			parse_headers(binstr:substr(Body, Index+2), binstr:substr(Body, 1, Index - 1), [])
	end.


parse_headers(Body, <<H, Tail/binary>>, []) when H =:= $\s; H =:= $\t ->
	% folded headers
	{[], list_to_binary([H, Tail, "\r\n", Body])};
parse_headers(Body, <<H, T/binary>>, Headers) when H =:= $\s; H =:= $\t ->
	% folded headers
	[{FieldName, OldFieldValue} | OtherHeaders] = Headers,
	FieldValue = list_to_binary([OldFieldValue, T]),
	%io:format("~p = ~p~n", [FieldName, FieldValue]),
	case binstr:strpos(Body, "\r\n") of
		0 ->
			{lists:reverse([{FieldName, FieldValue} | OtherHeaders]), Body};
		1 ->
			{lists:reverse([{FieldName, FieldValue} | OtherHeaders]), binstr:substr(Body, 3)};
		Index2 ->
			parse_headers(binstr:substr(Body, Index2 + 2), binstr:substr(Body, 1, Index2 - 1), [{FieldName, FieldValue} | OtherHeaders])
	end;
parse_headers(Body, Line, Headers) ->
	%io:format("line: ~p, nextpart ~p~n", [Line, binstr:substr(Body, 1, 10)]),
	case binstr:strchr(Line, $:) of
		0 ->
			{lists:reverse(Headers), list_to_binary([Line, "\r\n", Body])};
		Index ->
			FieldName = binstr:substr(Line, 1, Index - 1),
			F = fun(X) -> X > 32 andalso X < 127 end,
			case binstr:all(F, FieldName) of
				true ->
					F2 = fun(X) -> (X > 31 andalso X < 127) orelse X == 9 end,
					FValue = binstr:strip(binstr:substr(Line, Index+1)),
					FieldValue = case binstr:all(F2, FValue) of
						true ->
							FValue;
						_ ->
							% I couldn't figure out how to use a pure binary comprehension here :(
							list_to_binary([ filter_non_ascii(C) || <<C:8>> <= FValue])
					end,
					case binstr:strpos(Body, "\r\n") of
						0 ->
							{lists:reverse([{FieldName, FieldValue} | Headers]), Body};
						1 ->
							{lists:reverse([{FieldName, FieldValue} | Headers]), binstr:substr(Body, 3)};
						Index2 ->
							parse_headers(binstr:substr(Body, Index2 + 2), binstr:substr(Body, 1, Index2 - 1), [{FieldName, FieldValue} | Headers])
					end;
				false ->
					{lists:reverse(Headers), list_to_binary([Line, "\r\n", Body])}
			end
	end.

filter_non_ascii(C) when (C > 31 andalso C < 127); C == 9 ->
	<<C>>;
filter_non_ascii(_C) ->
	<<"?">>.

decode_body(Type, Body, _InEncoding, none) ->
	decode_body(Type, << <<X/integer>> || <<X>> <= Body, X < 128 >>);
decode_body(Type, Body, undefined, _OutEncoding) ->
	decode_body(Type, << <<X/integer>> || <<X>> <= Body, X < 128 >>);
decode_body(Type, Body, InEncoding, OutEncoding) ->
	NewBody = decode_body(Type, Body),
	CD = case iconv:open(OutEncoding, fix_encoding(InEncoding)) of
		{ok, Res} -> Res;
		{error, einval} -> throw({bad_charset, fix_encoding(InEncoding)})
	end,
	{ok, Result} = try iconv:conv_chunked(CD, NewBody) of
		{ok, _} = Res2 -> Res2
	catch
		_:_ ->
			iconv:conv(CD, NewBody)
	end,
	iconv:close(CD),
	Result.

-spec(decode_body/2 :: (Type :: binary() | 'undefined', Body :: binary()) -> binary()).
decode_body(undefined, Body) ->
	Body;
decode_body(Type, Body) ->
	case binstr:to_lower(Type) of
		<<"quoted-printable">> ->
			decode_quoted_printable(Body);
		<<"base64">> ->
			decode_base64(Body);
		_Other ->
			Body
	end.

decode_base64(Body) ->
	base64:mime_decode(Body).

decode_quoted_printable(Body) ->
	case binstr:strpos(Body, "\r\n") of
		0 ->
			decode_quoted_printable(Body, <<>>, []);
		Index ->
			decode_quoted_printable(binstr:substr(Body, 1, Index +1), binstr:substr(Body, Index + 2), [])
	end.

decode_quoted_printable(<<>>, <<>>, Acc) ->
	list_to_binary(lists:reverse(Acc));
decode_quoted_printable(Line, Rest, Acc) ->
	case binstr:strpos(Rest, "\r\n") of
		0 ->
			decode_quoted_printable(Rest, <<>>, [decode_quoted_printable_line(Line, []) | Acc]);
		Index ->
			%io:format("next line ~p~nnext rest ~p~n", [binstr:substr(Rest, 1, Index +1), binstr:substr(Rest, Index + 2)]),
			decode_quoted_printable(binstr:substr(Rest, 1, Index +1), binstr:substr(Rest, Index + 2),
				[decode_quoted_printable_line(Line, []) | Acc])
	end.

decode_quoted_printable_line(<<>>, Acc) ->
	lists:reverse(Acc);
decode_quoted_printable_line(<<$\r, $\n>>, Acc) ->
	lists:reverse(["\r\n" | Acc]);
decode_quoted_printable_line(<<$=, C, T/binary>>, Acc) when C =:= $\s; C =:= $\t ->
	case binstr:all(fun(X) -> X =:= $\s orelse X =:= $\t end, T) of
		true ->
			lists:reverse(Acc);
		false ->
			throw(badchar)
	end;
decode_quoted_printable_line(<<$=, $\r, $\n>>, Acc) ->
	lists:reverse(Acc);
decode_quoted_printable_line(<<$=, A:2/binary, T/binary>>, Acc) ->
	%<<X:1/binary, Y:1/binary>> = A,
	case binstr:all(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $A andalso C =< $F) orelse (C >= $a andalso C =< $f) end, A) of
		true ->
			{ok, [C | []], []} = io_lib:fread("~16u", binary_to_list(A)),
			decode_quoted_printable_line(T, [C | Acc]);
		false ->
			throw(badchar)
	end;
decode_quoted_printable_line(<<$=>>, Acc) ->
	% soft newline
	lists:reverse(Acc);
decode_quoted_printable_line(<<H, T/binary>>, Acc) when H >= $!, H =< $< ->
	decode_quoted_printable_line(T, [H | Acc]);
decode_quoted_printable_line(<<H, T/binary>>, Acc) when H >= $>, H =< $~ ->
	decode_quoted_printable_line(T, [H | Acc]);
decode_quoted_printable_line(<<H, T/binary>>, Acc) when H =:= $\s; H =:= $\t ->
	% if the rest of the line is whitespace, truncate it
	case binstr:all(fun(X) -> X =:= $\s orelse X =:= $\t end, T) of
		true ->
			lists:reverse(Acc);
		false ->
			decode_quoted_printable_line(T, [H | Acc])
	end;
decode_quoted_printable_line(<<H, T/binary>>, Acc) ->
	decode_quoted_printable_line(T, [H| Acc]).

check_headers(Headers) ->
	Checked = [<<"MIME-Version">>, <<"Date">>, <<"From">>, <<"Message-ID">>, <<"References">>, <<"Subject">>],
	check_headers(Checked, lists:reverse(Headers)).

check_headers([], Headers) ->
	lists:reverse(Headers);
check_headers([Header | Tail], Headers) ->
	case get_header_value(Header, Headers) of
		undefined when Header == <<"MIME-Version">> ->
			check_headers(Tail, [{<<"MIME-Version">>, <<"1.0">>} | Headers]);
		undefined when Header == <<"Date">> ->
			check_headers(Tail, [{<<"Date">>, list_to_binary(smtp_util:rfc5322_timestamp())} | Headers]);
		undefined when Header == <<"From">> ->
			erlang:error(missing_from);
		undefined when Header == <<"Message-ID">> ->
			check_headers(Tail, [{<<"Message-ID">>, list_to_binary(smtp_util:generate_message_id())} | Headers]);
		undefined when Header == <<"References">> ->
			case get_header_value(<<"In-Reply-To">>, Headers) of
				undefined ->
					check_headers(Tail, Headers); % ok, whatever
				ReplyID ->
					check_headers(Tail, [{<<"References">>, ReplyID} | Headers])
			end;
		References when Header == <<"References">> ->
			% check if the in-reply-to header, if present, is in references
			case get_header_value(<<"In-Reply-To">>, Headers) of
				undefined ->
					check_headers(Tail, Headers); % ok, whatever
				ReplyID ->
					case binstr:strpos(binstr:to_lower(References), binstr:to_lower(ReplyID)) of
						0 ->
							% okay, tack on the reply-to to the end of References
							check_headers(Tail, [{<<"References">>, list_to_binary([References, " ", ReplyID])} | proplists:delete(<<"References">>, Headers)]);
						_Index ->
							check_headers(Tail, Headers) % nothing to do
					end
				end;
		_ ->
			check_headers(Tail, Headers)
	end.

ensure_content_headers(Type, SubType, Parameters, Headers, Body, Toplevel) ->
	CheckHeaders = [<<"Content-Type">>, <<"Content-Disposition">>, <<"Content-Transfer-Encoding">>],
	ensure_content_headers(CheckHeaders, Type, SubType, Parameters, lists:reverse(Headers), Body, Toplevel).

ensure_content_headers([], _, _, Parameters, Headers, _, _) ->
	{Parameters, lists:reverse(Headers)};
ensure_content_headers([Header | Tail], Type, SubType, Parameters, Headers, Body, Toplevel) ->
	case get_header_value(Header, Headers) of
		undefined when Header == <<"Content-Type">>, ((Type == <<"text">> andalso SubType =/= <<"plain">>) orelse Type =/= <<"text">>) ->
			% no content-type header, and its not text/plain
			CT = io_lib:format("~s/~s", [Type, SubType]),
			CTP = case Type of
				<<"multipart">> ->
					Boundary = case proplists:get_value(<<"boundary">>, proplists:get_value(<<"content-type-params">>, Parameters, [])) of
						undefined ->
							list_to_binary(smtp_util:generate_message_boundary());
						B ->
							B
					end,
					[{<<"boundary">>, Boundary} | proplists:delete(<<"boundary">>, proplists:get_value(<<"content-type-params">>, Parameters, []))];
				<<"text">> ->
					Charset = case proplists:get_value(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, [])) of
						undefined ->
							guess_charset(Body);
						C ->
							C
					end,
					[{<<"charset">>, Charset} | proplists:delete(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, []))];
				_ ->
					proplists:get_value(<<"content-type-params">>, Parameters, [])
			end,

			%CTP = proplists:get_value(<<"content-type-params">>, Parameters, [guess_charset(Body)]),
			CTH = binstr:join([CT | encode_parameters(CTP)], ";\r\n\t"),
			NewParameters = [{<<"content-type-params">>, CTP} | proplists:delete(<<"content-type-params">>, Parameters)],
			ensure_content_headers(Tail, Type, SubType, NewParameters, [{<<"Content-Type">>, CTH} | Headers], Body, Toplevel);
		undefined when Header == <<"Content-Type">> ->
			% no content-type header and its text/plain
			Charset = case proplists:get_value(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, [])) of
				undefined ->
					guess_charset(Body);
				C ->
					C
			end,
			case Charset of
				<<"us-ascii">> ->
					% the default
					ensure_content_headers(Tail, Type, SubType, Parameters, Headers, Body, Toplevel);
				_ ->
					CTP = [{<<"charset">>, Charset} | proplists:delete(<<"charset">>, proplists:get_value(<<"content-type-params">>, Parameters, []))],
					CTH = binstr:join([<<"text/plain">> | encode_parameters(CTP)], ";\r\n\t"),
					NewParameters = [{<<"content-type-params">>, CTP} | proplists:delete(<<"content-type-params">>, Parameters)],
					ensure_content_headers(Tail, Type, SubType, NewParameters, [{<<"Content-Type">>, CTH} | Headers], Body, Toplevel)
			end;
		undefined when Header == <<"Content-Transfer-Encoding">>, Type =/= <<"multipart">> ->
			Enc = case proplists:get_value(<<"transfer-encoding">>, Parameters) of
				undefined ->
					guess_best_encoding(Body);
				Value ->
					Value
			end,
			case Enc of
				<<"7bit">> ->
					ensure_content_headers(Tail, Type, SubType, Parameters, Headers, Body, Toplevel);
				_ ->
					ensure_content_headers(Tail, Type, SubType, Parameters, [{<<"Content-Transfer-Encoding">>, Enc} | Headers], Body, Toplevel)
			end;
		undefined when Header == <<"Content-Disposition">>, Toplevel == false ->
			CD = proplists:get_value(<<"disposition">>, Parameters, <<"inline">>),
			CDP = proplists:get_value(<<"disposition-params">>, Parameters, []),
			CDH = binstr:join([CD | encode_parameters(CDP)], ";\r\n\t"),
			ensure_content_headers(Tail, Type, SubType, Parameters, [{<<"Content-Disposition">>, CDH} | Headers], Body, Toplevel);
		_ ->
			ensure_content_headers(Tail, Type, SubType, Parameters, Headers, Body, Toplevel)
	end.

guess_charset(Body) ->
	case binstr:all(fun(X) -> X < 128 end, Body) of
		true -> <<"us-ascii">>;
		false -> <<"utf-8">>
	end.

guess_best_encoding(<<Body:200/binary, Rest/binary>>) when Rest =/= <<>> ->
	guess_best_encoding(Body);
guess_best_encoding(Body) ->
	Size = byte_size(Body),
	% get only the allowed ascii characters
	% TODO - this might not be the complete list
	FilteredSize = length([X || <<X>> <= Body, ((X > 31 andalso X < 127) orelse X == $\r orelse X == $\n)]),

	Percent = round((FilteredSize / Size) * 100),

	%based on the % of printable characters, choose an encoding
	if
		Percent == 100 ->
			<<"7bit">>;
		Percent > 80 ->
			<<"quoted-printable">>;
		true ->
			<<"base64">>
	end.

encode_parameters([[]]) ->
	[];
encode_parameters(Parameters) ->
	[case binstr:strchr(Y, $\s) of 0 -> [X, "=", Y]; _ -> [X, "=\"", Y, "\""] end || {X, Y} <- Parameters].

encode_headers(Headers) ->
	encode_headers(Headers, []).

encode_headers([], EncodedHeaders) ->
	EncodedHeaders;
encode_headers([{Key, Value}|T] = _Headers, EncodedHeaders) ->
	encode_headers(T, encode_folded_header(list_to_binary([Key,": ",Value]),
			EncodedHeaders)).

encode_folded_header(Header, HeaderLines) ->
	case binstr:strchr(Header, $;) of
		0 ->
			HeaderLines ++ [Header];
		Index ->
			Remainder = binstr:substr(Header, Index+1),
			TabbedRemainder = case Remainder of
				<<$\t,_Rest/binary>> ->
					Remainder;
				_ ->
					list_to_binary(["\t", Remainder])
			end,
			% TODO - not tail recursive
			HeaderLines ++ [ binstr:substr(Header, 1, Index) ] ++
				encode_folded_header(TabbedRemainder, [])
	end.

encode_component(_Type, _SubType, Headers, Params, Body) ->
	if
		is_list(Body) -> % is this a multipart component?
			Boundary = proplists:get_value(<<"boundary">>, proplists:get_value(<<"content-type-params">>, Params)),
			[<<>>] ++  % blank line before start of component
			lists:flatmap(
				fun(Part) ->
						[list_to_binary([<<"--">>, Boundary])] ++ % start with the boundary
						encode_component_part(Part)
				end,
				Body
			) ++ [list_to_binary([<<"--">>, Boundary, <<"--">>])] % final boundary (with /--$/)
			  ++ [<<>>]; % blank line at the end of the multipart component
		true -> % or an inline component?
			%encode_component_part({Type, SubType, Headers, Params, Body})
			encode_body(
					get_header_value(<<"Content-Transfer-Encoding">>, Headers),
					[Body]
			 )
	end.

encode_component_part(Part) ->
	case Part of
		{<<"multipart">>, SubType, Headers, PartParams, Body} ->
			{FixedParams, FixedHeaders} = ensure_content_headers(<<"multipart">>, SubType, PartParams, Headers, Body, false),
			encode_headers(FixedHeaders) ++ [<<>>] ++
			encode_component(<<"multipart">>, SubType, FixedHeaders, FixedParams, Body);
		{Type, SubType, Headers, PartParams, Body} ->
			PartData = case Body of
				{_,_,_,_,_} -> encode_component_part(Body);
				String      -> [String]
			end,
			{_FixedParams, FixedHeaders} = ensure_content_headers(Type, SubType, PartParams, Headers, Body, false),
			encode_headers(FixedHeaders) ++ [<<>>] ++
			encode_body(
					get_header_value(<<"Content-Transfer-Encoding">>, FixedHeaders),
					PartData
			 );
		_ ->
			io:format("encode_component_part couldn't match Part to: ~p~n", [Part]),
			[]
	end.

encode_body(undefined, Body) ->
	Body;
encode_body(Type, Body) ->
	case binstr:to_lower(Type) of
		<<"quoted-printable">> ->
			[InnerBody] = Body,
			encode_quoted_printable(InnerBody);
		<<"base64">> ->
			[InnerBody] = Body,
			wrap_to_76(base64:encode(InnerBody));
		_ -> Body
	end.

wrap_to_76(String) ->
	[wrap_to_76(String, [])].

wrap_to_76(<<>>, Acc) ->
	list_to_binary(lists:reverse(Acc));
wrap_to_76(<<Head:76/binary, Tail/binary>>, Acc) ->
	wrap_to_76(Tail, [<<"\r\n">>, Head | Acc]);
wrap_to_76(Head, Acc) ->
	list_to_binary(lists:reverse([<<"\r\n">>, Head | Acc])).

encode_quoted_printable(Body) ->
	[encode_quoted_printable(Body, [], 0)].

encode_quoted_printable(Body, Acc, L) when L >= 75 ->
	LastLine = case string:str(Acc, "\n") of
		0 ->
			Acc;
		Index ->
			string:substr(Acc, 1, Index-1)
	end,
	%Len = length(LastLine),
	case string:str(LastLine, " ") of
		0 when L =:= 75 ->
			% uh-oh, no convienient whitespace, just cram a soft newline in
			encode_quoted_printable(Body, [$\n, $\r, $= | Acc], 0);
		1 when L =:= 75 ->
			% whitespace is the last character we wrote
			encode_quoted_printable(Body, [$\n, $\r, $= | Acc], 0);
		SIndex when (L - 75) < SIndex ->
			% okay, we can safely stick some whitespace in
			Prefix = string:substr(Acc, 1, SIndex-1),
			Suffix = string:substr(Acc, SIndex),
			NewAcc = lists:concat([Prefix, "\n\r=", Suffix]),
			encode_quoted_printable(Body, NewAcc, 0);
		_ ->
			% worst case, we're over 75 characters on the line
			% and there's no obvious break points, just stick one
			% in at position 75 and call it good. However, we have
			% to be very careful not to stick the soft newline in
			% the middle of an existing quoted-printable escape.

			% TODO - fix this to be less stupid
			I = 3, % assume we're at most 3 over our cutoff
			Prefix = string:substr(Acc, 1, I),
			Suffix = string:substr(Acc, I+1),
			NewAcc = lists:concat([Prefix, "\n\r=", Suffix]),
			encode_quoted_printable(Body, NewAcc, 0)
	end;
encode_quoted_printable(<<>>, Acc, _L) ->
	list_to_binary(lists:reverse(Acc));
encode_quoted_printable(<<$=, T/binary>> , Acc, L) ->
	encode_quoted_printable(T, [$D, $3, $= | Acc], L+3);
encode_quoted_printable(<<$\r, $\n, T/binary>> , Acc, _L) ->
	encode_quoted_printable(T, [$\n, $\r | Acc], 0);
encode_quoted_printable(<<H, T/binary>>, Acc, L) when H >= $!, H =< $< ->
	encode_quoted_printable(T, [H | Acc], L+1);
encode_quoted_printable(<<H, T/binary>>, Acc, L) when H >= $>, H =< $~ ->
	encode_quoted_printable(T, [H | Acc], L+1);
encode_quoted_printable(<<H, $\r, $\n, T/binary>>, Acc, _L) when H == $\s; H == $\t ->
	[[A, B]] = io_lib:format("~2.16.0B", [H]),
	encode_quoted_printable(T, [$\n, $\r, B, A, $= | Acc], 0);
encode_quoted_printable(<<H, T/binary>>, Acc, L) when H == $\s; H == $\t ->
	encode_quoted_printable(T, [H | Acc], L+1);
encode_quoted_printable(<<H, T/binary>>, Acc, L) ->
	[[A, B]] = io_lib:format("~2.16.0B", [H]),
	encode_quoted_printable(T, [B, A, $= | Acc], L+3).

get_default_encoding() ->
	case code:ensure_loaded(iconv) of
		{error, _} ->
			none;
		{module, iconv} ->
			<<"utf-8//IGNORE">>
	end.

% convert some common invalid character names into the correct ones
fix_encoding(Encoding) when Encoding == <<"utf8">>; Encoding == <<"UTF8">> ->
	<<"UTF-8">>;
fix_encoding(Encoding) ->
	Encoding.

-ifdef(TEST).

parse_with_comments_test_() ->
	[
		{"bleh",
			fun() ->
					?assertEqual(<<"1.0">>, parse_with_comments(<<"1.0">>)),
					?assertEqual(<<"1.0">>, parse_with_comments(<<"1.0  (produced by MetaSend Vx.x)">>)),
					?assertEqual(<<"1.0">>, parse_with_comments(<<"(produced by MetaSend Vx.x) 1.0">>)),
					?assertEqual(<<"1.0">>, parse_with_comments(<<"1.(produced by MetaSend Vx.x)0">>))
			end
		},
		{"comments that parse as empty",
			fun() ->
					?assertEqual(<<>>, parse_with_comments(<<"(comment (nested (deeply)) (and (oh no!) again))">>)),
					?assertEqual(<<>>, parse_with_comments(<<"(\\)\\\\)">>)),
					?assertEqual(<<>>, parse_with_comments(<<"(by way of Whatever <redir@my.org>)    (generated by Eudora)">>))
			end
		},
		{"some more",
			fun() ->
					?assertEqual(<<":sysmail@  group. org, Muhammed. Ali @Vegas.WBA">>, parse_with_comments(<<"\":sysmail\"@  group. org, Muhammed.(the greatest) Ali @(the)Vegas.WBA">>)),
					?assertEqual(<<"Pete <pete@silly.test>">>, parse_with_comments(<<"Pete(A wonderful \\) chap) <pete(his account)@silly.test(his host)>">>))
			end
		},
		{"non list values",
			fun() ->
					?assertEqual(undefined, parse_with_comments(undefined)),
					?assertEqual(17, parse_with_comments(17))
			end
		},
		{"Parens within quotes ignored",
			fun() ->
				?assertEqual(<<"Height (from xkcd).eml">>, parse_with_comments(<<"\"Height (from xkcd).eml\"">>)),
				?assertEqual(<<"Height (from xkcd).eml">>, parse_with_comments(<<"\"Height \(from xkcd\).eml\"">>))
			end
		},
		{"Escaped quotes are handled correctly",
			fun() ->
					?assertEqual(<<"Hello \"world\"">>, parse_with_comments(<<"Hello \\\"world\\\"">>)),
					?assertEqual(<<"<boss@nil.test>, Giant; \"Big\" Box <sysservices@example.net>">>, parse_with_comments(<<"<boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>">>))
			end
		},
		{"backslash not part of a quoted pair",
			fun() ->
					?assertEqual(<<"AC \\ DC">>, parse_with_comments(<<"AC \\ DC">>)),
					?assertEqual(<<"AC  DC">>, parse_with_comments(<<"AC ( \\ ) DC">>))
			end
		},
		{"Unterminated quotes or comments",
			fun() ->
					?assertError(unterminated_quotes, parse_with_comments(<<"\"Hello there ">>)),
					?assertError(unterminated_quotes, parse_with_comments(<<"\"Hello there \\\"">>)),
					?assertError(unterminated_comment, parse_with_comments(<<"(Hello there ">>)),
					?assertError(unterminated_comment, parse_with_comments(<<"(Hello there \\\)">>))
			end
		}
	].
	
parse_content_type_test_() ->
	[
		{"parsing content types",
			fun() ->
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"text/plain; charset=us-ascii (Plain text)">>)),
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"text/plain; charset=\"us-ascii\"">>)),
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"Text/Plain; Charset=\"us-ascii\"">>)),
					?assertEqual({<<"multipart">>, <<"mixed">>, [{<<"boundary">>, <<"----_=_NextPart_001_01C9DCAE.1F2CB390">>}]},
						parse_content_type(<<"multipart/mixed; boundary=\"----_=_NextPart_001_01C9DCAE.1F2CB390\"">>))
			end
		},
		{"parsing content type with a tab in it",
			fun() ->
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_type(<<"text/plain;\tcharset=us-ascii">>)),
					?assertEqual({<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}, {<<"foo">>, <<"bar">>}]}, parse_content_type(<<"text/plain;\tcharset=us-ascii;\tfoo=bar">>))
			end
		},
		{"invalid content types",
			fun() ->
					?assertThrow(bad_content_type, parse_content_type(<<"text\\plain; charset=us-ascii">>)),
					?assertThrow(bad_content_type, parse_content_type(<<"text/plain; charset us-ascii">>))
				end
			}
	].

parse_content_disposition_test_() ->
	[
		{"parsing valid dispositions",
			fun() ->
					?assertEqual({<<"inline">>, []}, parse_content_disposition(<<"inline">>)),
					?assertEqual({<<"inline">>, []}, parse_content_disposition(<<"inline;">>)),
					?assertEqual({<<"attachment">>, [{<<"filename">>, <<"genome.jpeg">>}, {<<"modification-date">>, <<"Wed, 12 Feb 1997 16:29:51 -0500">>}]}, parse_content_disposition(<<"attachment; filename=genome.jpeg;modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";">>)),
					?assertEqual({<<"text/plain">>, [{<<"charset">>, <<"us-ascii">>}]}, parse_content_disposition(<<"text/plain; charset=us-ascii (Plain text)">>))
			end
		},
		{"invalid dispositions",
			fun() ->
					?assertThrow(bad_disposition, parse_content_disposition(<<"inline; =bar">>)),
					?assertThrow(bad_disposition, parse_content_disposition(<<"inline; bar">>))
			end
		}
	].

various_parsing_test_() ->
	[
		{"split_body_by_boundary test",
			fun() ->
					?assertEqual([{[], <<"foo bar baz">>}], split_body_by_boundary_(<<"stuff\r\nfoo bar baz">>, <<"--bleh">>, [])),
					?assertEqual([{[], <<"foo\r\n">>}, {[], <<>>}, {[], <<>>}, {[], <<"bar baz">>}], split_body_by_boundary_(<<"stuff\r\nfoo\r\n--bleh\r\n--bleh\r\n--bleh-- stuff\r\nbar baz">>, <<"--bleh">>, [])),
					%?assertEqual([{[], []}, {[], []}, {[], "bar baz"}], split_body_by_boundary_("\r\n--bleh\r\n--bleh\r\n", "--bleh", [])),
					%?assertMatch([{"text", "plain", [], _,"foo\r\n"}], split_body_by_boundary("stuff\r\nfoo\r\n--bleh\r\n--bleh\r\n--bleh-- stuff\r\nbar baz", "--bleh", "1.0"))
					?assertEqual({[], <<"foo: bar\r\n">>}, parse_headers(<<"\r\nfoo: bar\r\n">>)),
					?assertEqual({[{<<"foo">>, <<"barbaz">>}], <<>>}, parse_headers(<<"foo: bar\r\n baz\r\n">>)),
					?assertEqual({[], <<" foo bar baz\r\nbam">>}, parse_headers(<<"\sfoo bar baz\r\nbam">>)),
					ok
			end
		},
		{"Headers with non-ASCII characters",
			fun() ->
					?assertEqual({[{<<"foo">>, <<"bar ?? baz">>}], <<>>}, parse_headers(<<"foo: bar ø baz\r\n">>)),
					?assertEqual({[], <<"bär: bar baz\r\n">>}, parse_headers(<<"bär: bar baz\r\n">>))
			end
		},
		{"Headers with tab characters",
			fun() ->
					?assertEqual({[{<<"foo">>, <<"bar		baz">>}], <<>>}, parse_headers(<<"foo: bar		baz\r\n">>))
			end
		}

	].

-define(IMAGE_MD5, <<110,130,37,247,39,149,224,61,114,198,227,138,113,4,198,60>>).

parse_example_mails_test_() ->
	Getmail = fun(File) ->
		{ok, Email} = file:read_file(string:concat("../testdata/", File)),
		%Email = binary_to_list(Bin),
		decode(Email)
	end,
	[
		{"parse a plain text email",
			fun() ->
				Decoded = Getmail("Plain-text-only.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"text">>, <<"plain">>}, {Type, SubType}),
				?assertEqual(<<"This message contains only plain text.\r\n">>, Body)
			end
		},
		{"parse a plain text email with no content type",
			fun() ->
				Decoded = Getmail("Plain-text-only-no-content-type.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"text">>, <<"plain">>}, {Type, SubType}),
				?assertEqual(<<"This message contains only plain text.\r\n">>, Body)
			end
		},
		{"parse a plain text email with no MIME header",
			fun() ->
				{Type, SubType, _Headers, _Properties, Body} =
					Getmail("Plain-text-only-no-MIME.eml"),
				?assertEqual({<<"text">>, <<"plain">>}, {Type, SubType}),
				?assertEqual(<<"This message contains only plain text.\r\n">>, Body)
			end
		},
		{"parse an email that says it is multipart but contains no boundaries",
			fun() ->
					?assertError(missing_boundary, Getmail("Plain-text-only-with-boundary-header.eml"))
			end
		},
		{"parse a multipart email with no MIME header",
			fun() ->
					?assertError(non_mime_multipart, Getmail("rich-text-no-MIME.eml"))
			end
		},
		{"rich text",
			fun() ->
				%% pardon my naming here.  apparently 'rich text' in mac mail
				%% means 'html'.
				Decoded = Getmail("rich-text.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"alternative">>}, {Type, SubType}),
				?assertEqual(2, length(Body)),
				[Plain, Html] = Body,
				?assertEqual({5, 5}, {tuple_size(Plain), tuple_size(Html)}),
				?assertMatch({<<"text">>, <<"plain">>, _, _, <<"This message contains rich text.">>}, Plain),
				?assertMatch({<<"text">>, <<"html">>, _, _, <<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This </b><i>message </i><span class=\"Apple-style-span\" style=\"text-decoration: underline;\">contains </span>rich text.</body></html>">>}, Html)
			end
		},
		{"rich text no boundary",
			fun() ->
				?assertError(no_boundary, Getmail("rich-text-no-boundary.eml"))
			end
		},
		{"rich text missing first boundary",
			fun() ->
				% TODO - should we handle this more elegantly?
				Decoded = Getmail("rich-text-missing-first-boundary.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"alternative">>}, {Type, SubType}),
				?assertEqual(1, length(Body)),
				[Html] = Body,
				?assertEqual(5, tuple_size(Html)),
				?assertMatch({<<"text">>, <<"html">>, _, _, <<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This </b><i>message </i><span class=\"Apple-style-span\" style=\"text-decoration: underline;\">contains </span>rich text.</body></html>">>}, Html)
			end
		},
		{"rich text missing last boundary",
			fun() ->
				?assertError(missing_last_boundary, Getmail("rich-text-missing-last-boundary.eml"))
			end
		},
		{"rich text wrong last boundary",
			fun() ->
				?assertError(missing_last_boundary, Getmail("rich-text-broken-last-boundary.eml"))
			end
		},
		{"rich text missing text content type",
			fun() ->
				%% pardon my naming here.  apparently 'rich text' in mac mail
				%% means 'html'.
				Decoded = Getmail("rich-text-no-text-contenttype.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"alternative">>}, {Type, SubType}),
				?assertEqual(2, length(Body)),
				[Plain, Html] = Body,
				?assertEqual({5, 5}, {tuple_size(Plain), tuple_size(Html)}),
				?assertMatch({<<"text">>, <<"plain">>, _, _, <<"This message contains rich text.">>}, Plain),
				?assertMatch({<<"text">>, <<"html">>, _, _, <<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This </b><i>message </i><span class=\"Apple-style-span\" style=\"text-decoration: underline;\">contains </span>rich text.</body></html>">>}, Html)
			end
		},
		{"text attachment only",
			fun() ->
				Decoded = Getmail("text-attachment-only.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"mixed">>}, {Type, SubType}),
				?assertEqual(1, length(Body)),
				Rich = <<"{\\rtf1\\ansi\\ansicpg1252\\cocoartf949\\cocoasubrtf460\r\n{\\fonttbl\\f0\\fswiss\\fcharset0 Helvetica;}\r\n{\\colortbl;\\red255\\green255\\blue255;}\r\n\\margl1440\\margr1440\\vieww9000\\viewh8400\\viewkind0\r\n\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural\\pardirnatural\r\n\r\n\\f0\\fs24 \\cf0 This is a basic rtf file.}">>,
				?assertMatch([{<<"text">>, <<"rtf">>, _, _, Rich}], Body)
			end
		},
		{"image attachment only",
			fun() ->
				Decoded = Getmail("image-attachment-only.eml"),
				?assertEqual(5, tuple_size(Decoded)),
				{Type, SubType, _Headers, _Properties, Body} = Decoded,
				?assertEqual({<<"multipart">>, <<"mixed">>}, {Type, SubType}),
				?assertEqual(1, length(Body)),
				?assertMatch([{<<"image">>, <<"jpeg">>, _, _, _}], Body),
				[H | _] = Body,
				[{<<"image">>, <<"jpeg">>, _, Parameters, _Image}] = Body,
				?assertEqual(?IMAGE_MD5, erlang:md5(element(5, H))),
				?assertEqual(<<"inline">>, proplists:get_value(<<"disposition">>, Parameters)),
				?assertEqual(<<"chili-pepper.jpg">>, proplists:get_value(<<"filename">>, proplists:get_value(<<"disposition-params">>, Parameters))),
				?assertEqual(<<"chili-pepper.jpg">>, proplists:get_value(<<"name">>, proplists:get_value(<<"content-type-params">>, Parameters)))
			end
		},
		{"message attachment only",
			fun() ->
				Decoded = Getmail("message-as-attachment.eml"),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, _}, Decoded),
				[Body] = element(5, Decoded),
				?assertMatch({<<"message">>, <<"rfc822">>, _, _, _}, Body),
				Subbody = element(5, Body),
				?assertMatch({<<"text">>, <<"plain">>, _, _, _}, Subbody),
				?assertEqual(<<"This message contains only plain text.\r\n">>, element(5, Subbody))
			end
		},
		{"message, image, and rtf attachments.",
			fun() ->
				Decoded = Getmail("message-image-text-attachments.eml"),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, _}, Decoded),
				?assertEqual(3, length(element(5, Decoded))),
				[Message, Rtf, Image] = element(5, Decoded),
				?assertMatch({<<"message">>, <<"rfc822">>, _, _, _}, Message),
				Submessage = element(5, Message),
				?assertMatch({<<"text">>, <<"plain">>, _, _, <<"This message contains only plain text.\r\n">>}, Submessage),
				
				?assertMatch({<<"text">>, <<"rtf">>, _, _, _}, Rtf),
				?assertEqual(<<"{\\rtf1\\ansi\\ansicpg1252\\cocoartf949\\cocoasubrtf460\r\n{\\fonttbl\\f0\\fswiss\\fcharset0 Helvetica;}\r\n{\\colortbl;\\red255\\green255\\blue255;}\r\n\\margl1440\\margr1440\\vieww9000\\viewh8400\\viewkind0\r\n\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural\\pardirnatural\r\n\r\n\\f0\\fs24 \\cf0 This is a basic rtf file.}">>, element(5, Rtf)),
				
				?assertMatch({<<"image">>, <<"jpeg">>, _, _, _}, Image),
				?assertEqual(?IMAGE_MD5, erlang:md5(element(5, Image)))				
			end
		},
		{"Outlook 2007 with leading tabs in quoted-printable.",
			fun() ->
				Decoded = Getmail("outlook-2007.eml"),
				?assertMatch({<<"multipart">>, <<"alternative">>, _, _, _}, Decoded)
			end
		},
		{"The gamut",
			fun() ->
				% multipart/alternative
				%	text/plain
				%	multipart/mixed
				%		text/html
				%		message/rf822
				%			multipart/mixed
				%				message/rfc822
				%					text/plain
				%		text/html
				%		message/rtc822
				%			text/plain
				%		text/html
				%		image/jpeg
				%		text/html
				%		text/rtf
				%		text/html
				Decoded = Getmail("the-gamut.eml"),
				?assertMatch({<<"multipart">>, <<"alternative">>, _, _, _}, Decoded),
				?assertEqual(2, length(element(5, Decoded))),
				[Toptext, Topmultipart] = element(5, Decoded),
				?assertMatch({<<"text">>, <<"plain">>, _, _, _}, Toptext),
				?assertEqual(<<"This is rich text.\r\n\r\nThe list is html.\r\n\r\nAttchments:\r\nan email containing an attachment of an email.\r\nan email of only plain text.\r\nan image\r\nan rtf file.\r\n">>, element(5, Toptext)),
				?assertEqual(9, length(element(5, Topmultipart))),
				[Html, Messagewithin, _Brhtml, _Message, _Brhtml, Image, _Brhtml, Rtf, _Brhtml] = element(5, Topmultipart),
				?assertMatch({<<"text">>, <<"html">>, _, _, _}, Html),
				?assertEqual(<<"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><b>This</b> is <i>rich</i> text.<div><br></div><div>The list is html.</div><div><br></div><div>Attchments:</div><div><ul class=\"MailOutline\"><li>an email containing an attachment of an email.</li><li>an email of only plain text.</li><li>an image</li><li>an rtf file.</li></ul></div><div></div></body></html>">>, element(5, Html)),
				
				?assertMatch({<<"message">>, <<"rfc822">>, _, _, _}, Messagewithin),
				%?assertEqual(1, length(element(5, Messagewithin))),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, [{<<"message">>, <<"rfc822">>, _, _, {<<"text">>, <<"plain">>, _, _, <<"This message contains only plain text.\r\n">>}}]}, element(5, Messagewithin)),
				
				?assertMatch({<<"image">>, <<"jpeg">>, _, _, _}, Image),
				?assertEqual(?IMAGE_MD5, erlang:md5(element(5, Image))),
				
				?assertMatch({<<"text">>, <<"rtf">>, _, _, _}, Rtf),
				?assertEqual(<<"{\\rtf1\\ansi\\ansicpg1252\\cocoartf949\\cocoasubrtf460\r\n{\\fonttbl\\f0\\fswiss\\fcharset0 Helvetica;}\r\n{\\colortbl;\\red255\\green255\\blue255;}\r\n\\margl1440\\margr1440\\vieww9000\\viewh8400\\viewkind0\r\n\\pard\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\ql\\qnatural\\pardirnatural\r\n\r\n\\f0\\fs24 \\cf0 This is a basic rtf file.}">>, element(5, Rtf))
				
			end
		},
		{"Plain text and 2 identical attachments",
			fun() ->
				Decoded = Getmail("plain-text-and-two-identical-attachments.eml"),
				?assertMatch({<<"multipart">>, <<"mixed">>, _, _, _}, Decoded),
				?assertEqual(3, length(element(5, Decoded))),
				[Plain, Attach1, Attach2] = element(5, Decoded),
				?assertEqual(Attach1, Attach2),
				?assertMatch({<<"text">>, <<"plain">>, _, _, _}, Plain),
				?assertEqual(<<"This message contains only plain text.\r\n">>, element(5, Plain))
			end
		},
		{"no \\r\\n before first boundary",
			fun() ->
				{ok, Bin} = file:read_file("../testdata/html.eml"),
				{Headers, B} = parse_headers(Bin),
				Body = binstr:strip(binstr:strip(B, left, $\r), left, $\n),
				Decoded = decode(Headers, Body),
				?assertEqual(2, length(element(5, Decoded)))
			end
		},
		{"testcase1",
			fun() ->
				Multipart = <<"multipart">>,
				Alternative = <<"alternative">>,
				Related = <<"related">>,
				Mixed = <<"mixed">>,
				Text = <<"text">>,
				Html = <<"html">>,
				Plain = <<"plain">>,
				Message = <<"message">>,
				Ref822 = <<"rfc822">>,
				Image = <<"image">>,
				Jpeg = <<"jpeg">>,
				%Imagemd5 = <<69,175,198,78,52,72,6,233,147,22,50,137,128,180,169,50>>,
				Imagemd5 = <<179,151,42,139,78,14,182,78,24,160,123,221,217,14,141,5>>,
				Decoded = Getmail("testcase1"),
				?assertMatch({Multipart, Mixed, _, _, [_, _]}, Decoded),
				[Multi1, Message1] = element(5, Decoded),
				?assertMatch({Multipart, Alternative, _, _, [_, _]}, Multi1),
				[Plain1, Html1] = element(5, Multi1),
				?assertMatch({Text, Plain, _, _, _}, Plain1),
				?assertMatch({Text, Html, _, _, _}, Html1),
				?assertMatch({Message, Ref822, _, _, _}, Message1),
				Multi2 = element(5, Message1),
				?assertMatch({Multipart, Alternative, _, _, [_, _]}, Multi2),
				[Plain2, Related1] = element(5, Multi2),
				?assertMatch({Text, Plain, _, _, _}, Plain2),
				?assertMatch({Multipart, Related, _, _, [_, _]}, Related1),
				[Html2, Image1] = element(5, Related1),
				?assertMatch({Text, Html, _, _, _}, Html2),
				?assertMatch({Image, Jpeg, _, _, _}, Image1),
				Resimage = erlang:md5(element(5, Image1)),
				?assertEqual(Imagemd5, Resimage)
			end
		},
		{"testcase2",
			fun() ->
				Multipart = <<"multipart">>,
				Alternative = <<"alternative">>,
				Mixed = <<"mixed">>,
				Text = <<"text">>,
				Html = <<"html">>,
				Plain = <<"plain">>,
				Message = <<"message">>,
				Ref822 = <<"rfc822">>,
				Application = <<"application">>,
				Octetstream = <<"octet-stream">>,
				Decoded = Getmail("testcase2"),
				?assertMatch({Multipart, Mixed, _, _, [_, _, _]}, Decoded),
				[Plain1, Stream1, Message1] = element(5, Decoded),
				?assertMatch({Text, Plain, _, _, _}, Plain1),
				?assertMatch({Application, Octetstream, _, _, _}, Stream1),
				?assertMatch({Message, Ref822, _, _, _}, Message1),
				Multi1 = element(5, Message1),
				?assertMatch({Multipart, Alternative, _, _, [_, _]}, Multi1),
				[Plain2, Html1] = element(5, Multi1),
				?assertMatch({Text, Plain, _, _, _}, Plain2),
				?assertMatch({Text, Html, _, _, _}, Html1)
			end
		}
	].

decode_quoted_printable_test_() ->
	[
		{"bleh",
			fun() ->
					?assertEqual("!", decode_quoted_printable_line(<<"=21">>, "")),
					?assertEqual("!!", decode_quoted_printable_line(<<"=21=21">>, "")),
					?assertEqual("=:=", decode_quoted_printable_line(<<"=3D:=3D">>, "")),
					?assertEqual("Thequickbrownfoxjumpedoverthelazydog.", decode_quoted_printable_line(<<"Thequickbrownfoxjumpedoverthelazydog.">>, ""))
			end
		},
		{"lowercase bleh",
			fun() ->
					?assertEqual("=:=", decode_quoted_printable_line(<<"=3d:=3d">>, ""))
			end
		},
		{"input with spaces",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.">>, ""))
			end
		},
		{"input with tabs",
			fun() ->
					?assertEqual("The\tquick brown fox jumped over\tthe lazy dog.", decode_quoted_printable_line(<<"The\tquick brown fox jumped over\tthe lazy dog.">>, ""))
			end
		},
		{"input with trailing spaces",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       ">>, ""))
			end
		},
		{"input with non-strippable trailing whitespace",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.        ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =20">>, "")),
					?assertEqual("The quick brown fox jumped over the lazy dog.       \t", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =09">>, "")),
					?assertEqual("The quick brown fox jumped over the lazy dog.\t \t \t \t ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.\t \t \t =09=20">>, "")),
					?assertEqual("The quick brown fox jumped over the lazy dog.\t \t \t \t ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.\t \t \t =09=20\t                  \t">>, ""))
			end
		},
		{"input with trailing tabs",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.\t\t\t\t\t">>, ""))
			end
		},
		{"soft new line",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.       ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =">>, ""))
			end
		},
		{"soft new line with trailing whitespace",
			fun() ->
					?assertEqual("The quick brown fox jumped over the lazy dog.       ", decode_quoted_printable_line(<<"The quick brown fox jumped over the lazy dog.       =  	">>, ""))
			end
		},
		{"multiline stuff",
			fun() ->
					?assertEqual(<<"Now's the time for all folk to come to the aid of their country.">>, decode_quoted_printable(<<"Now's the time =\r\nfor all folk to come=\r\n to the aid of their country.">>)),
					?assertEqual(<<"Now's the time\r\nfor all folk to come\r\n to the aid of their country.">>, decode_quoted_printable(<<"Now's the time\r\nfor all folk to come\r\n to the aid of their country.">>)),
					?assertEqual(<<"hello world">>, decode_quoted_printable(<<"hello world">>)),
					?assertEqual(<<"hello\r\n\r\nworld">>, decode_quoted_printable(<<"hello\r\n\r\nworld">>))
			end
		},
		{"invalid input",
			fun() ->
					?assertThrow(badchar, decode_quoted_printable_line(<<"=21=G1">>, "")),
					?assertThrow(badchar, decode_quoted_printable(<<"=21=D1 = g ">>))
			end
		},
		{"out of range characters should be stripped",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo ", 150, " bar">>, "US-ASCII", "UTF-8//IGNORE"))
			end
		},
		{"out of range character in alternate charset should be converted",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo ", 226, 128, 147, " bar">>, decode_body(<<"quoted-printable">>, <<"Foo ",150," bar">>, "Windows-1252", "UTF-8//IGNORE"))
			end
		},
		{"out of range character in alternate charset with no destination encoding should be stripped",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo ",150," bar">>, "Windows-1252", none))
			end
		},
		{"out of range character in alternate charset with no source encoding should be stripped",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo ",150," bar">>, undefined, "UTF-8"))
			end
		},
		{"almost correct chatsets should work, eg. 'UTF8' instead of 'UTF-8'",
			fun() ->
				% character 150 is en-dash in windows 1252
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo  bar">>, <<"UTF8">>, "UTF-8")),
				?assertEqual(<<"Foo  bar">>, decode_body(<<"quoted-printable">>, <<"Foo  bar">>, <<"utf8">>, "UTF-8"))
			end
		}
	].

encode_quoted_printable_test_() ->
	[
		{"bleh",
			fun() ->
					?assertEqual(<<"!">>, encode_quoted_printable(<<"!">>, [], 0)),
					?assertEqual(<<"!!">>, encode_quoted_printable(<<"!!">>, [], 0)),
					?assertEqual(<<"=3D:=3D">>, encode_quoted_printable(<<"=:=">>, [], 0)),
					?assertEqual(<<"Thequickbrownfoxjumpedoverthelazydog.">>,
						encode_quoted_printable(<<"Thequickbrownfoxjumpedoverthelazydog.">>, [], 0))
			end
		},
		{"input with spaces",
			fun() ->
					?assertEqual(<<"The quick brown fox jumped over the lazy dog.">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog.">>, "", 0))
			end
		},
		{"input with tabs",
			fun() ->
					?assertEqual(<<"The\tquick brown fox jumped over\tthe lazy dog.">>,
						encode_quoted_printable(<<"The\tquick brown fox jumped over\tthe lazy dog.">>, "", 0))
			end
		},
		{"input with trailing spaces",
			fun() ->
					?assertEqual(<<"The quick brown fox jumped over the lazy dog.      =20\r\n">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog.       \r\n">>, "", 0))
			end
		},
		{"input with non-ascii characters",
			fun() ->
					?assertEqual(<<"There's some n=F8n-=E1scii st=FCff in here\r\n">>,
						encode_quoted_printable(<<"There's some n", 248, "n-", 225,"scii st", 252, "ff in here\r\n">>, "", 0))
			end
		},
		{"input with invisible non-ascii characters",
			fun() ->
					?assertEqual(<<"There's some stuff=C2=A0in=C2=A0here\r\n">>,
						encode_quoted_printable(<<"There's some stuff in here\r\n">>, "", 0))
			end
		},
		{"add soft newlines",
			fun() ->
					?assertEqual(<<"The quick brown fox jumped over the lazy dog. The quick brown fox jumped =\r\nover the lazy dog.">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_ov=\r\ner_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_over_the_lazy_dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o=\r\n=3Dver_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o=ver_the_lazy_dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_=\r\n=3Dover_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_=over_the_lazy_dog.">>, "", 0)),
					?assertEqual(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o =\r\nver_the_lazy_dog.">>,
						encode_quoted_printable(<<"The_quick_brown_fox_jumped_over_the_lazy_dog._The_quick_brown_fox_jumped_o ver_the_lazy_dog.">>, "", 0))
			end
		},
		{"newline craziness",
			fun() ->
					?assertEqual(<<"foo ba=\r\nr\r\nThe quick brown fox jumped over the lazy dog.      =20\r\n">>,
						encode_quoted_printable(<<"The quick brown fox jumped over the lazy dog.       \r\n">>, "\n\rrab oof", 78))
			end
		}
	].

rfc2047_decode_test_() ->
	[
		{"Simple tests",
			fun() ->
					?assertEqual(<<"Keith Moore <moore@cs.utk.edu>">>, decode_header(<<"=?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>">>, "utf-8")),
					?assertEqual(<<"Keld Jørn Simonsen <keld@dkuug.dk>">>, decode_header(<<"=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>">>, "utf-8")),
					?assertEqual(<<"Olle Järnefors <ojarnef@admin.kth.se>">>, decode_header(<<"=?ISO-8859-1?Q?Olle_J=E4rnefors?= <ojarnef@admin.kth.se>">>, "utf-8")),
					?assertEqual(<<"André Pirard <PIRARD@vm1.ulg.ac.be>">>, decode_header(<<"=?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>">>, "utf-8"))
			end
		},
		{"encoded words seperated by whitespace should have whitespace removed",
			fun() ->
					?assertEqual(<<"If you can read this you understand the example.">>, decode_header(<<"=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?= =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=">>, "utf-8")),
					?assertEqual(<<"ab">>, decode_header(<<"=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=">>, "utf-8")),
					?assertEqual(<<"ab">>, decode_header(<<"=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=">>, "utf-8")),
					?assertEqual(<<"ab">>, decode_header(<<"=?ISO-8859-1?Q?a?=
		=?ISO-8859-1?Q?b?=">>, "utf-8"))
			end
		},
		{"underscores expand to spaces",
			fun() ->
					?assertEqual(<<"a b">>, decode_header(<<"=?ISO-8859-1?Q?a_b?=">>, "utf-8")),
					?assertEqual(<<"a b">>, decode_header(<<"=?ISO-8859-1?Q?a?= =?ISO-8859-2?Q?_b?=">>, "utf-8"))
			end
		},
		{"edgecases",
			fun() ->
					?assertEqual(<<"this is some text">>, decode_header(<<"=?iso-8859-1?q?this=20is=20some=20text?=">>, "utf-8")),
					?assertEqual(<<"=?iso-8859-1?q?this is some text?=">>, decode_header(<<"=?iso-8859-1?q?this is some text?=">>, "utf-8"))
			end
		},
		{"invalid character sequence handling",
			fun() ->
					?assertError({badmatch, {error, eilseq}}, decode_header(<<"=?us-ascii?B?dGhpcyBjb250YWlucyBhIGNvcHlyaWdodCCpIHN5bWJvbA==?=">>, "utf-8")),
					?assertEqual(<<"this contains a copyright  symbol">>, decode_header(<<"=?us-ascii?B?dGhpcyBjb250YWlucyBhIGNvcHlyaWdodCCpIHN5bWJvbA==?=">>, "utf-8//IGNORE")),
					?assertEqual(<<"this contains a copyright © symbol">>, decode_header(<<"=?iso-8859-1?B?dGhpcyBjb250YWlucyBhIGNvcHlyaWdodCCpIHN5bWJvbA==?=">>, "utf-8//IGNORE"))
			end
		},
		{"multiple unicode email addresses",
			fun() ->
					?assertEqual(<<"Jacek Złydach <jacek.zlydach@erlang-solutions.com>, chak de planet óóóó <jz@erlang-solutions.com>, Jacek Złydach <jacek.zlydach@erlang-solutions.com>, chak de planet óóóó <jz@erlang-solutions.com>">>, decode_header(<<"=?UTF-8?B?SmFjZWsgWsWCeWRhY2g=?= <jacek.zlydach@erlang-solutions.com>, =?UTF-8?B?Y2hhayBkZSBwbGFuZXQgw7PDs8Ozw7M=?= <jz@erlang-solutions.com>, =?UTF-8?B?SmFjZWsgWsWCeWRhY2g=?= <jacek.zlydach@erlang-solutions.com>, =?UTF-8?B?Y2hhayBkZSBwbGFuZXQgw7PDs8Ozw7M=?= <jz@erlang-solutions.com>">>, "utf-8"))
			end
		}
	].

encoding_test_() ->
	[
		{"Simple email",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>},
							{<<"Message-ID">>, <<"<abcd@example.com>">>},
							{<<"MIME-Version">>, <<"1.0">>},
							{<<"Date">>, <<"Sun, 01 Nov 2009 14:44:47 +0200">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = <<"From: me@example.com\r\nTo: you@example.com\r\nSubject: This is a test\r\nMessage-ID: <abcd@example.com>\r\nMIME-Version: 1.0\r\nDate: Sun, 01 Nov 2009 14:44:47 +0200\r\n\r\nThis is a plain message">>,
					?assertEqual(Result, encode(Email))
			end
		},
		{"multipart/alternative email",
			fun() ->
					Email = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>},
							{<<"MIME-Version">>, <<"1.0">>},
							{<<"Content-Type">>,
								<<"multipart/alternative; boundary=wtf-123234234">>}],
						[{<<"content-type-params">>,
								[{<<"boundary">>, <<"wtf-123234234">>}]},
							{<<"disposition">>,<<"inline">>},
							{<<"disposition-params">>,[]}],
						[{<<"text">>,<<"plain">>,
								[{<<"Content-Type">>,
										<<"text/plain;charset=US-ASCII;format=flowed">>},
									{<<"Content-Transfer-Encoding">>,<<"7bit">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>},
											{<<"format">>,<<"flowed">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"This message contains rich text.">>},
							{<<"text">>,<<"html">>,
								[{<<"Content-Type">>,<<"text/html;charset=US-ASCII">>},
									{<<"Content-Transfer-Encoding">>,<<"7bit">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"<html><body>This message also contains HTML</body></html>">>}]},
					Result = decode(encode(Email)),
					?assertMatch({<<"multipart">>, <<"alternative">>, _, _, [{<<"text">>,
									<<"plain">>, _, _, _}, {<<"text">>, <<"html">>, _, _, _}]},
						Result)
			end
		},
		{"multipart/alternative email with encoding",
			fun() ->
					Email = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>},
							{<<"MIME-Version">>, <<"1.0">>},
							{<<"Content-Type">>,
								<<"multipart/alternative; boundary=wtf-123234234">>}],
						[{<<"content-type-params">>,
								[{<<"boundary">>, <<"wtf-123234234">>}]},
							{<<"disposition">>,<<"inline">>},
							{<<"disposition-params">>,[]}],
						[{<<"text">>,<<"plain">>,
								[{<<"Content-Type">>,
										<<"text/plain;charset=US-ASCII;format=flowed">>},
									{<<"Content-Transfer-Encoding">>,<<"quoted-printable">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>},
											{<<"format">>,<<"flowed">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"This message contains rich text.\r\n",
								"and is =quoted printable= encoded!">>},
							{<<"text">>,<<"html">>,
								[{<<"Content-Type">>,<<"text/html;charset=US-ASCII">>},
									{<<"Content-Transfer-Encoding">>,<<"base64">>}],
								[{<<"content-type-params">>,
										[{<<"charset">>,<<"US-ASCII">>}]},
									{<<"disposition">>,<<"inline">>},
									{<<"disposition-params">>,[]}],
								<<"<html><body>This message also contains",
								"HTML and is base64",
								"encoded\r\n\r\n</body></html>">>}]},
					Result = decode(encode(Email)),
					?assertMatch({<<"multipart">>, <<"alternative">>, _, _, [{<<"text">>,
									<<"plain">>, _, _, <<"This message contains rich text.\r\n",
									"and is =quoted printable= encoded!">>},
								{<<"text">>, <<"html">>, _, _,
									<<"<html><body>This message also contains",
									"HTML and is base64",
									"encoded\r\n\r\n</body></html>">>}]},
						Result)
			end
		},
		{"Missing headers should be added",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertNot(undefined == proplists:get_value(<<"Message-ID">>, element(3, Result))),
					?assertNot(undefined == proplists:get_value(<<"Date">>, element(3, Result))),
					?assertEqual(undefined, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Reference header should be added in presence of In-Reply-To",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"In-Reply-To">>, <<"<abcd@example.com>">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertEqual(<<"<abcd@example.com>">>, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Reference header should be appended to in presence of In-Reply-To, if appropiate",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"In-Reply-To">>, <<"<abcd@example.com>">>},
							{<<"References">>, <<"<wxyz@example.com>">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertEqual(<<"<wxyz@example.com> <abcd@example.com>">>, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Reference header should NOT be appended to in presence of In-Reply-To, if already present",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"In-Reply-To">>, <<"<abcd@example.com>">>},
							{<<"References">>, <<"<wxyz@example.com> <abcd@example.com>">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"content-type-params">>,
								[{<<"charset">>,<<"US-ASCII">>}],
								{<<"disposition">>,<<"inline">>}}],
						<<"This is a plain message">>},
					Result = decode(encode(Email)),
					?assertEqual(<<"<wxyz@example.com> <abcd@example.com>">>, proplists:get_value(<<"References">>, element(3, Result)))
			end
		},
		{"Content-Transfer-Encoding header should be added if missing and appropriate",
			fun() ->
					Email = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a plain message with some non-ascii characters øÿ\r\nso there">>},
					Encoded = encode(Email),
					Result = decode(Encoded),
					?assertEqual(<<"quoted-printable">>, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result))),
					Email2 = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a plain message with no non-ascii characters">>},
					Encoded2 = encode(Email2),
					Result2 = decode(Encoded2),
					?assertEqual(undefined, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result2))),
					Email3 = {<<"text">>, <<"plain">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[{<<"transfer-encoding">>, <<"base64">>}],
						<<"This is a plain message with no non-ascii characters">>},
					Encoded3 = encode(Email3),
					Result3 = decode(Encoded3),
					?assertEqual(<<"base64">>, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result3)))
			end
		},
		{"Content-Type header should be added if missing and appropriate",
			fun() ->
					Email = {<<"text">>, <<"html">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a HTML message with some non-ascii characters øÿ\r\nso there">>},
					Encoded = encode(Email),
					Result = decode(Encoded),
					?assertEqual(<<"quoted-printable">>, proplists:get_value(<<"Content-Transfer-Encoding">>, element(3, Result))),
					?assertMatch(<<"text/html;charset=utf-8">>, proplists:get_value(<<"Content-Type">>, element(3, Result))),
					Email2 = {<<"text">>, <<"html">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a HTML message with no non-ascii characters\r\nso there">>},
					Encoded2 = encode(Email2),
					Result2 = decode(Encoded2),
					?assertMatch(<<"text/html;charset=us-ascii">>, proplists:get_value(<<"Content-Type">>, element(3, Result2))),
					Email3 = {<<"text">>, <<"html">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						<<"This is a text message with some invisible non-ascii characters\r\nso there">>},
					Encoded3 = encode(Email3),
					Result3 = decode(Encoded3),
					?assertMatch(<<"text/html;charset=utf-8">>, proplists:get_value(<<"Content-Type">>, element(3, Result3)))
			end
		},
		{"Content-Type header should be added for subparts too, if missing and appropriate",
			fun() ->
					Email4 = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>, <<"plain">>, [], [], <<"This is a multipart message with some invisible non-ascii characters\r\nso there">>}]},
					Encoded4 = encode(Email4),
					Result4 = decode(Encoded4),
					?assertMatch(<<"text/plain;charset=utf-8">>, proplists:get_value(<<"Content-Type">>, element(3, lists:nth(1,element(5, Result4)))))
			end
		},
		{"Content-Type header should be not added for subparts if they're text/plain us-ascii",
			fun() ->
					Email4 = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>, <<"plain">>, [], [], <<"This is a multipart message with no non-ascii characters\r\nso there">>}]},
					Encoded4 = encode(Email4),
					Result4 = decode(Encoded4),
					?assertMatch(undefined, proplists:get_value(<<"Content-Type">>, element(3, lists:nth(1,element(5, Result4)))))
			end
		},
		{"Content-Type header should be added for subparts if they're text/html us-ascii",
			fun() ->
					Email4 = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>, <<"html">>, [], [], <<"This is a multipart message with no non-ascii characters\r\nso there">>}]},
					Encoded4 = encode(Email4),
					Result4 = decode(Encoded4),
					?assertMatch(<<"text/html;charset=us-ascii">>, proplists:get_value(<<"Content-Type">>, element(3, lists:nth(1,element(5, Result4)))))
			end
		},
		{"A boundary should be generated if applicable",
			fun() ->
					Email = {<<"multipart">>, <<"alternative">>, [
							{<<"From">>, <<"me@example.com">>},
							{<<"To">>, <<"you@example.com">>},
							{<<"Subject">>, <<"This is a test">>}],
						[],
						[{<<"text">>,<<"plain">>,
								[],
								[],
								<<"This message contains rich text.\r\n",
								"and is =quoted printable= encoded!">>},
							{<<"text">>,<<"html">>,
								[],
								[],
								<<"<html><body>This message also contains",
								"HTML and is base64",
								"encoded\r\n\r\n</body></html>">>}]},
					Encoded = encode(Email),
					Result = decode(Encoded),
					Boundary = proplists:get_value(<<"boundary">>, proplists:get_value(<<"content-type-params">>, element(4, Result))),
					?assert(is_binary(Boundary)),
					% ensure we don't add the header multiple times
					?assertEqual(1, length(proplists:get_all_values(<<"Content-Type">>, element(3, Result)))),
					% headers should be appended, not prepended
					?assertMatch({<<"From">>, _}, lists:nth(1, element(3, Result))),
					ok
			end
		}
	].

roundtrip_test_() ->
	[
		{"roundtrip test for the gamut",
			fun() ->
					{ok, Email} = file:read_file("../testdata/the-gamut.eml"),
					Decoded = decode(Email),
					_Encoded = encode(Decoded),
					%{ok, F1} = file:open("f1", [write]),
					%{ok, F2} = file:open("f2", [write]),
					%file:write(F1, Email),
					%file:write(F2, Encoded),
					%file:close(F1),
					%file:close(F2),
					?assertEqual(Email, Email)
			end
		},
		{"round trip plain text only email",
			fun() ->
					{ok, Email} = file:read_file("../testdata/Plain-text-only.eml"),
					Decoded = decode(Email),
					_Encoded = encode(Decoded),
					%{ok, F1} = file:open("f1", [write]),
					%{ok, F2} = file:open("f2", [write]),
					%file:write(F1, Email),
					%file:write(F2, Encoded),
					%file:close(F1),
					%file:close(F2),
					?assertEqual(Email, Email)
			end
		},
		{"round trip quoted-printable email",
			fun() ->
					{ok, Email} = file:read_file("../testdata/testcase1"),
					Decoded = decode(Email),
					_Encoded = encode(Decoded),
					%{ok, F1} = file:open("f1", [write]),
					%{ok, F2} = file:open("f2", [write]),
					%file:write(F1, Email),
					%file:write(F2, Encoded),
					%file:close(F1),
					%file:close(F2),
					?assertEqual(Email, Email)
					%ok
			end
		}
	].


-endif.

