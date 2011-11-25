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
    
header(Header, Req) ->
    Req:get_header_value(string:to_lower(Header)).

headers(Req) ->
    F = fun(Header) -> Req:get_header_value(Header) end,
    Headers1 = [
        {connection, F("connection")},
        {accept, F("accept")},
        {host, F("host")},
        {if_modified_since, F("if-modified-since")},
        {if_match, F("if-match")},
        {if_none_match, F("if-range")},
        {if_unmodified_since, F("if-unmodified-since")},
        {range, F("range")},
        {referer, F("referer")},
        {user_agent, F("user-agent")},
        {accept_language, F("accept-language")},
        {accept_ranges, F("accept-ranges")},
        {cookie, F("cookie")},
        {keep_alive, F("keep-alive")},
        {location, F("location")},
        {content_length, F("content-length")},
        {content_type, F("content-type")},
        {content_encoding, F("content-encoding")},
        {authorization, F("authorization")},
        {x_forwarded_for, F("x-forwarded-for")},
        {transfer_encoding, F("transfer-encoding")}
    ],
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
