-module(boss_csrf_middleware).
-export([before_/3, after_/4]).

before_(Req, _SessionID, Info) ->
    [CSRF_Token, NewToken] = get_csrf_token(Req, Info),
    {ok, Data} = case lists:member(Req:request_method(), ['GET', 'HEAD', 'OPTIONS', 'TRACE']) of
        true -> accept_(Req, NewToken);
        false ->
            case proplists:is_defined(do_not_enforce_csrf_checks, Info) of
                true -> accept_(Req, NewToken);
                false ->
                    case check_referer(Req) of
                        false -> reject_(Req, NewToken, incorrect_referer);
                        true ->
                            pre_check_csrf_token(Req, CSRF_Token, NewToken)
                    end
            end
    end,
    %%{ok, Data, [{cookie, io_lib:format("csrftoken=~s", [NewToken])}]}.
    {ok, Data}.

after_(_Req, _SessionId, Result, _Headers) ->
    io:format("Result so far: ~p.~n", [Result]),
    %%{ok, [{csrf_token, get_random_string(12)}]}.
    {ok, [], [{cookie, "test=123"}]}.

pre_check_csrf_token(Req, CSRF_Token, NewToken) ->
    %% Pre check CSRF token
    %%case CSRF_Token =:= none of
    case NewToken =:= none of
        true ->
            reject_(Req, NewToken, no_csrf_cookie);
        false ->
            case Req:post_param("csrfmiddlewaretoken") of
                undefined ->
                    %% No CSRF Token in POST, let's check HEADERS
                    case proplists:get_value(http_x_csrftoken, Req:headers()) of
                        undefined -> reject_(Req, NewToken, no_csrf_token);
                        HTTPToken ->
                            check_csrf_token(Req, HTTPToken, CSRF_Token, NewToken)
                    end;
                PostToken ->
                    %% We have token, let's check it
                    check_csrf_token(Req, PostToken, CSRF_Token, NewToken)
            end
    end.

check_csrf_token(Req, PostToken, CSRF_Token, NewToken) ->
    case PostToken =:= CSRF_Token of
        true ->
            accept_(Req, NewToken);
        false ->
            reject_(Req, NewToken, incorrect_csrf_token)
    end.



get_csrf_token(Req, Info) ->
    case Req:cookie(csrftoken) of
        undefined ->
            %% Generate New
            [none, get_random_string(12)];
        Token ->
            [Token, Token]
    end.

get_random_string(Length) ->
    get_random_string(Length, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

check_referer(Req) ->
    %% Check referer, only on secure requests
    case proplists:get_value(referer, Req:headers()) of
        undefined ->
            %%false;
            true;
        Referer -> %% TODO: Finish referer check
            true
    end.

accept_(_Req, Token) ->
    {ok, [{csrftoken, Token}]}.

reject_(_Req, Token, Reason) ->
    io:format("~n~n~nREJECTED!!!!!~nReason: ~p~n~n~n", [Reason]),
    {ok, [{csrftoken, Token}]}.
