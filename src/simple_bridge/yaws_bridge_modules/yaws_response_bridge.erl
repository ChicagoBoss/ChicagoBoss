% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(yaws_response_bridge).
-behaviour (simple_bridge_response).
-include_lib ("../include/yaws_api.hrl").
-include_lib ("../include/simple_bridge.hrl").
-export ([build_response/2]).

build_response(_Arg, Res) ->
    % Get vars...
    Code = Res#response.statuscode,
    case Res#response.data of
        {data, Body} ->
            % Assemble headers...
            Headers = lists:flatten([
                [{header, {X#header.name, X#header.value}} || X <- Res#response.headers],
                [create_cookie(X) || X <- Res#response.cookies]
            ]),

            % Get the content type...
            ContentType = coalesce([
                kvl3(content_type, Res#response.headers),
                kvl3("content-type", Res#response.headers),
                kvl3("Content-Type", Res#response.headers),
                kvl3("CONTENT-TYPE", Res#response.headers),
                "text/html"
            ]),

            % Send the yaws response...
            lists:flatten([
                {status, Code},
                Headers,
                {content, ContentType, Body}
            ]);

        {file, Path} ->
            %% Calculate expire date far into future...
            Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
            TenYears = 10 * 365 * 24 * 60 * 60,
            Seconds1 = calendar:gregorian_seconds_to_datetime(Seconds + TenYears),
            ExpireDate = httpd_util:rfc1123_date(Seconds1),

            % Create the response telling Yaws to server file...
            Options = [{header, {"Expires", ExpireDate}}],
            Path = filename:join(".", Path),
            {page, {Options, Path}}
    end.

kvl3(Key,L) ->
    case lists:keysearch(Key,2,L) of
        {value, {_,_,Val}} -> Val;
        _                  -> undefined
    end.

coalesce([]) -> undefined;
coalesce([undefined|T]) -> coalesce(T);
coalesce([H|_T]) -> H.

create_cookie(Cookie) ->
    Name = Cookie#cookie.name,
    Value = Cookie#cookie.value,
    Path = Cookie#cookie.path,
    SecondsToLive = Cookie#cookie.minutes_to_live * 60,
    Expire = to_cookie_expire(SecondsToLive),
    yaws_api:setcookie(Name, Value, Path, Expire).

to_cookie_expire(SecondsToLive) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
    httpd_util:rfc1123_date(DateTime).
