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
            Headers = Mod:headers(Req),
            proplists:get_value(Header, Headers)
    end.

cookies() -> Mod:cookies(Req).

cookie(Cookie) ->
    case erlang:function_exported(Mod, cookie, 2) of
        true -> 
            Mod:cookie(Cookie, Req);
        false ->
            Cookies = Mod:cookies(Req),
            proplists:get_value(Cookie, Cookies)
    end.

session_id() ->
    cookie(boss_session:get_session_key()).

query_params() -> Mod:query_params(Req).

query_param(Param) ->
    proplists:get_value(Param, query_params()).

post_params() -> 
    case {request_method(), IsMultiPart} of
        {'POST', true}  -> PostParams;
        {'POST', false} -> Mod:post_params(Req);
        _ -> []
    end.

post_param(Param) ->
    proplists:get_value(Param, post_params()).

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

deep_post_params() ->
    Params = post_params(),
    parse_deep_post_params(Params, []).

deep_post_param(Path) ->
    find_deep_post_param(Path, deep_post_params()).

find_deep_post_param([], Params) ->
    Params;
find_deep_post_param([Index|Rest], Params) when is_integer(Index) ->
    find_deep_post_param(Rest, lists:nth(Index, Params));
find_deep_post_param([Index|Rest], Params) when is_list(Index) ->
    find_deep_post_param(Rest, proplists:get_value(Index, Params)).

parse_deep_post_params([], Acc) ->
    Acc;
parse_deep_post_params([{Key, Value}|Rest], Acc) ->
    case re:run(Key, "^(\\w+)(?:\\[([\\w\\[\\]]+)\\])?$", [{capture, all_but_first, list}]) of
        {match, [Key]} ->
            parse_deep_post_params(Rest, [{Key, Value}|Acc]);
        {match, [KeyName, Path]} ->
            PathList = re:split(Path, "\\]\\[", [{return, list}]),
            parse_deep_post_params(Rest, insert_into(Acc, [KeyName|PathList], Value))
    end.

insert_into(_List, [], Value) ->
    Value;
insert_into(undefined, PathList, Value) ->
    insert_into([], PathList, Value);
insert_into(N, PathList, Value) when is_integer(N) ->
    insert_into([], PathList, Value);
insert_into(List, [ThisKey|Rest], Value) ->
    case catch list_to_integer(ThisKey) of
        {'EXIT', _} ->
            ExistingVal = proplists:get_value(ThisKey, List),
            [{ThisKey, insert_into(ExistingVal, Rest, Value)}|
                proplists:delete(ThisKey, List)];
        N when N < erlang:length(List) ->
            ExistingVal = lists:nth(N+1, List),
            lists:sublist(List, N) ++ [insert_into(ExistingVal, Rest, Value)|
                lists:nthtail(N+1, List)];
        N when N >= erlang:length(List) ->
            List ++ lists:reverse([insert_into(undefined, Rest, Value)|
                    lists:seq(0, N - erlang:length(List) - 1)])
    end.
