% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mochiweb_request_bridge).
-behaviour (simple_bridge_request).
-include_lib ("simple_bridge.hrl").
-export ([
    init/1,
    request_method/1, path/1, uri/1,
    peer_ip/1, peer_port/1,
    headers/1, header/2, cookies/1,
    query_params/1, post_params/1, request_body/1,
    socket/1, recv_from_socket/3
]).

init(Req) -> 
    Req.

request_method(Req) -> 
    Req:get(method).

path(Req) -> 
    RawPath = Req:get(raw_path),
    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),
    Path.

uri(Req) ->
    Req:get(raw_path).

peer_ip(Req) -> 
    case Req:get(socket) of
        false -> {127, 0, 0, 1};
        Socket ->
            {ok, {IP, _Port}} = mochiweb_socket:peername(Socket),
            IP
    end.

peer_port(Req) -> 
    Socket = Req:get(socket),
    {ok, {_IP, Port}} = mochiweb_socket:peername(Socket),
    Port.
    
header(connection, Req) ->
    Req:get_header_value("connection");
header(accept, Req) ->
    Req:get_header_value("accept");
header(host, Req) ->
    Req:get_header_value("host");
header(if_modified_since, Req) ->
    Req:get_header_value("if-modified-since");
header(if_match, Req) ->
    Req:get_header_value("if-match");
header(if_none_match, Req) ->
    Req:get_header_value("if-none-match");
header(if_unmodified_since, Req) ->
    Req:get_header_value("if-unmodified-since");
header(if_range, Req) ->
    Req:get_header_value("if-range");
header(range, Req) ->
    Req:get_header_value("range");
header(user_agent, Req) ->
    Req:get_header_value("user-agent");
header(accept_language, Req) ->
    Req:get_header_value("accept-language");
header(accept_ranges, Req) ->
    Req:get_header_value("accept-ranges");
header(cookie, Req) ->
    Req:get_header_value("cookie");
header(keep_alive, Req) ->
    Req:get_header_value("keep-alive");
header(location, Req) ->
    Req:get_header_value("location");
header(content_length, Req) ->
    Req:get_header_value("content-length");
header(content_type, Req) ->
    Req:get_header_value("content-type");
header(content_encoding, Req) ->
    Req:get_header_value("content-encoding");
header(authorization, Req) ->
    Req:get_header_value("authorization");
header(x_forwarded_for, Req) ->
    Req:get_header_value("x-forwarded-for");
header(transfer_encoding, Req) ->
    Req:get_header_value("transfer-encoding");
header(Header, Req) ->
    Req:get_header_value(Header).

headers(Req) ->
    Headers = [connection, accept, host, if_modified_since,
        connection, accept, host, if_modified_since, if_match, 
        if_none_match, if_unmodified_since, if_range, range, 
        referer, user_agent, accept_language, accept_ranges, 
        cookie, keep_alive, location, content_length, content_type,
        content_encoding, authorization, x_forwarded_for, transfer_encoding
    ],
    Headers1 = lists:map(fun(H) -> {H, header(H, Req)} end, Headers),
    [{K, V} || {K, V} <- Headers1, V /= undefined].

cookies(Req) ->
    Req:parse_cookie().

query_params(Req) ->
    Req:parse_qs().

post_params(Req) ->
    Req:parse_post().

request_body(Req) ->
    Req:recv_body().

socket(Req) -> 	
    Req:get(socket).

recv_from_socket(Length, Timeout, Req) -> 
    Socket = socket(Req),
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} -> 
            put(mochiweb_request_recv, true),
            Data;
        _Other -> 
            exit(normal)
    end.
