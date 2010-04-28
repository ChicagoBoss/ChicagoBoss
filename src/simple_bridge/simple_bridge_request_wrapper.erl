% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge_request_wrapper, [Mod, Req, IsMultiPart, PostParams, PostFiles, Error]).
-compile(export_all).
-include_lib ("simple_bridge.hrl").

set_multipart(PostParams1, PostFiles1) ->
    simple_bridge_request_wrapper:new(Mod, Req, true, PostParams1, PostFiles1, Error).

set_error(Error1) ->
    simple_bridge_request_wrapper:new(Mod, Req, true, PostParams, PostFiles, Error1).

request_method() -> Mod:request_method(Req).
path() -> Mod:path(Req).
uri() -> Mod:uri(Req).

peer_ip() -> Mod:peer_ip(Req).
peer_port() -> Mod:peer_port(Req).

headers() -> Mod:headers(Req).

header(Header) -> 
    case erlang:function_exported(Mod, header, 2) of
        true -> 
            Mod:header(Header, Req);
        false ->
            proplists:get_value(Header, headers())
    end.

cookies() -> Mod:cookies(Req).

cookie(Cookie) ->
    case erlang:function_exported(Mod, cookie, 2) of
        true -> 
            Mod:cookie(Cookie, Req);
        false ->
            proplists:get_value(Cookie, cookies())
    end.

query_params() -> Mod:query_params(Req).

query_param(QueryParam) ->
    case erlang:function_exported(Mod, query_param, 2) of
        true -> 
            Mod:query_param(QueryParam, Req);
        false ->
            proplists:get_value(QueryParam, query_params())
    end.

post_params() -> 
    case {request_method(), IsMultiPart} of
        {'POST', true}  -> PostParams;
        {'POST', false} -> Mod:post_params(Req);
        _ -> []
    end.

post_param(PostParam) ->
    case erlang:function_exported(Mod, post_param, 2) of
        true -> 
            Mod:post_param(PostParam, Req);
        false ->
            proplists:get_value(PostParam, post_params())
    end.

post_files() -> PostFiles.

request_body() -> Mod:request_body(Req).

socket() -> 
    case erlang:function_exported(Mod, socket, 1) of
        true -> Mod:socket(Req);
        false -> throw({not_supported, Mod, socket})
    end.

get_peername() -> inet:peername(socket()).

recv_from_socket(Length, Timeout) -> 
    case erlang:function_exported(Mod, recv_from_socket, 3) of
        true ->  Mod:recv_from_socket(Length, Timeout, Req);
        false -> throw({not_supported, Mod, recv_from_socket})
    end.

error() -> Error.
