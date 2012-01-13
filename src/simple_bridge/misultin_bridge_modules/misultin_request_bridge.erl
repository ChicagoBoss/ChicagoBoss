-module (misultin_request_bridge).
-behaviour (simple_bridge_request).
-include_lib ("simple_bridge.hrl").
-export ([
    init/1,
    request_method/1, path/1, uri/1,
    peer_ip/1, peer_port/1,
    headers/1, header/2, cookies/1,
    query_params/1, post_params/1, request_body/1
]).


%% @todo could not figure out how to get the socket from misultin 
%% so could not implement socket/1, recv_from_socket/3 that are 
%% present in other request modules 

init(Req) -> 
    Req.

request_method(Req) -> 
    Req:get(method).

path(Req) -> 
    {abs_path,Path} = Req:get(uri),
    Path.

uri(Req) ->
    Req:get(uri).

peer_ip(Req) -> 
    case Req:get(peer_addr) of
        {ok, IP} -> IP;
        IP -> IP
    end.

peer_port(Req) -> 
    Req:get(peer_port).

header(connection, Req) ->
    misultin_utility:get_key_value('Connection', Req:get(headers));
header(accept, Req) ->
    misultin_utility:get_key_value('Accept', Req:get(headers));
header(host, Req) ->
    misultin_utility:get_key_value('Host', Req:get(headers));
header(if_modified_since, Req) ->
    misultin_utility:get_key_value('If-Modified-Since', Req:get(headers));
header(if_match, Req) ->
    misultin_utility:get_key_value('If-Match', Req:get(headers));
header(if_none_match, Req) ->
    misultin_utility:get_key_value('If-None-Match', Req:get(headers));
header(if_range, Req) ->
    misultin_utility:get_key_value('If-Range', Req:get(headers));
header(if_unmodified_since, Req) ->
    misultin_utility:get_key_value('If-Unmodified-Since', Req:get(headers));
header(range, Req) ->
    misultin_utility:get_key_value('Range', Req:get(headers));
header(referer, Req) ->
    misultin_utility:get_key_value('Referer', Req:get(headers));
header(user_agent, Req) ->
    misultin_utility:get_key_value('User-Agent', Req:get(headers));
header(accept_ranges, Req) ->
    misultin_utility:get_key_value('Accept-Ranges', Req:get(headers));
header(cookie, Req) ->
    misultin_utility:get_key_value('Cookie', Req:get(headers));
header(keep_alive, Req) ->
    misultin_utility:get_key_value('Keep-Alive', Req:get(headers));
header(location, Req) ->
    misultin_utility:get_key_value('Location', Req:get(headers));
header(content_length, Req) ->
    misultin_utility:get_key_value('Content-Length', Req:get(headers));
header(content_type, Req) ->
    misultin_utility:get_key_value('Content-Type', Req:get(headers));
header(content_encoding, Req) ->
    misultin_utility:get_key_value('Content-Encoding', Req:get(headers));
header(authorization, Req) ->
    misultin_utility:get_key_value('Authorization', Req:get(headers));
header(transfer_encoding, Req) ->
    misultin_utility:get_key_value('Transfer-Encoding', Req:get(headers));
header(Header, Req) ->
    misultin_utility:get_key_value(Header, Req:get(headers)).

headers(Req) ->
    Headers1 = [ connection, accept, host, if_modified_since, 
        if_match, if_none_match, if_range, if_unmodified_since, 
        range, referer, user_agent, accept_ranges, cookie, 
        keep_alive, location, content_length, content_type, 
        content_encoding, authorization, transfer_encoding
    ],
    Headers2 = lists:map(fun(H) -> {H, header(H, Req)} end, Headers1),
    [{K, V} || {K, V} <- Headers2, V /= undefined].

cookies(Req) ->
    Headers = headers(Req),
    CookieData = proplists:get_value(cookie, Headers, ""),
    F = fun(Cookie) ->
        case string:tokens(Cookie, "=") of
            [] -> [];
            L -> 
                X = string:strip(hd(L)),
                Y = string:join(tl(L), "="),
                {X, Y}
        end
    end,
    [F(X) || X <- string:tokens(CookieData, ";")].

query_params(Req) ->
    Req:parse_qs().	

post_params(Req) ->
    Req:parse_post().

request_body(Req) ->
    Req:get(body).
