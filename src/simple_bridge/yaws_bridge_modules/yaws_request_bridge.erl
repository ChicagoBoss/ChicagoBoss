% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(yaws_request_bridge).
-include_lib ("../include/yaws_api.hrl").
-include_lib ("../include/simple_bridge.hrl").
-export ([
    init/1,
    request_method/1, path/1, uri/1,
    peer_ip/1, peer_port/1,
    headers/1, cookie/2, cookies/1,
    query_params/1, post_params/1, request_body/1,
    socket/1, recv_from_socket/3
]).

init(Req) ->
    Req.

request_method(Arg) ->
    (Arg#arg.req)#http_request.method.

path(Arg) ->
    Arg#arg.server_path.

uri(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    Path.

peer_ip(Arg) -> 
    Socket = socket(Arg),
    {ok, {IP, _Port}} = inet:peername(Socket),
    IP.

peer_port(Arg) -> 
    Socket = socket(Arg),
    {ok, {_IP, Port}} = inet:peername(Socket),
    Port.

headers(Arg) ->
    Headers = Arg#arg.headers,
    [
        {connection, Headers#headers.connection},
        {accept, Headers#headers.accept},
        {host, Headers#headers.host},
        {if_modified_since, Headers#headers.if_modified_since},
        {if_match, Headers#headers.if_match},
        {if_none_match, Headers#headers.if_none_match},
        {if_range, Headers#headers.if_range},
        {if_unmodified_since, Headers#headers.if_unmodified_since},
        {range, Headers#headers.range},
        {referer, Headers#headers.referer},
        {user_agent, Headers#headers.user_agent},
        {accept_ranges, Headers#headers.accept_ranges},
        {cookie, Headers#headers.cookie},
        {keep_alive, Headers#headers.keep_alive},
        {location, Headers#headers.location},
        {content_length, Headers#headers.content_length},
        {content_type, Headers#headers.content_type},
        {content_encoding, Headers#headers.content_encoding},
        {authorization, Headers#headers.authorization},
        {transfer_encoding, Headers#headers.transfer_encoding}
    ].

cookie(Key, Req) ->
    Headers = Req#arg.headers,
    yaws_api:find_cookie_val(Key, Headers#headers.cookie).

cookies(Req) ->
    Headers = Req#arg.headers,
    CookieList = Headers#headers.cookie,
    F = fun(Cookie) ->
        Key = hd(string:tokens(Cookie, "=")),
        Val = yaws_api:find_cookie_val(Key, [Cookie]),
        {Key, Val}
    end,
    [F(X) || X <- CookieList].

query_params(Arg) ->
    yaws_api:parse_query(Arg).

post_params(Arg) ->
    yaws_api:parse_post(Arg).

request_body(Arg) ->
    case Arg#arg.clidata of
        {partial, Data} -> Data;
        Data -> Data
    end.  

socket(Arg) ->
    Arg#arg.clisock.

-ifndef(debug_print).
-define(debug_print, true).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

recv_from_socket(Length, Timeout, Arg) -> 
    Socket = socket(Arg),
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} -> Data;
        _ -> exit(normal)
    end.
