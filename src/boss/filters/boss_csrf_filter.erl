-module(boss_csrf_filter).
-export([before_filter/2, middle_filter/3, after_filter/3, csrftoken_name/0]).

-define(CSRFTOKEN_NAME, csrf_token).
-define(CSRFTOKEN_PARAM_NAME, "csrfmiddlewaretoken").
-define(CSRFTOKEN_HEADER, http_x_csrftoken).

before_filter(Config, RequestContext) ->
    %% Get Request
    Request = proplists:get_value(request, RequestContext),

    ReConfig = case Config =:= undefined of
                   true ->
                       RequestContext;
                   false ->
                       Config
               end,

    [CSRF_Token, NewToken] = get_csrf_token(Request),
    case lists:member(proplists:get_value(method, RequestContext),
                      ['GET', 'HEAD', 'OPTIONS', 'TRACE']) of
        true -> accept_(RequestContext, NewToken);
        false ->
            case proplists:is_defined(do_not_enforce_csrf_checks, ReConfig) of
                true -> accept_(RequestContext, NewToken);
                false ->
                    case check_referer(Request) of
                        false -> reject_(incorrect_referer);
                        true ->
                            pre_check_csrf_token(RequestContext, CSRF_Token, NewToken)
                    end
            end
    end.

middle_filter({render, ReturnValue, SomethingElse}, _, Context) ->
    %% Adds csrf_token variable to  template _before variable, even if it's not there
    TemplateTokenField = case proplists:get_value(?CSRFTOKEN_NAME, ReturnValue) of
                             undefined ->
                                 template_field(proplists:get_value(?CSRFTOKEN_NAME, Context, new_token()));
                             Value ->
                                 template_field(Value)
                         end,

    NewReturnValue = case proplists:get_value(?CSRFTOKEN_NAME, ReturnValue) of
                         undefined ->
                             ReturnValue;
                         _ ->
                             proplists:delete(?CSRFTOKEN_NAME, ReturnValue)
                     end,

    {render, [{atom_to_list(?CSRFTOKEN_NAME), TemplateTokenField} | NewReturnValue], SomethingElse};
middle_filter(Other, _, _) ->
    Other.



after_filter({Whatever, Content, Headers}, _, RequestContext) ->
    Token = proplists:get_value(?CSRFTOKEN_NAME, RequestContext, new_token()),

    %% Set ?CSRFTOKEN_NAME cookie
    {Whatever, Content, [
        mochiweb_cookies:cookie(
            ?CSRFTOKEN_NAME,
            Token,
          [{path, "/"}, {max_age, boss_session:get_session_exp_time()}]) | Headers]};
after_filter(Other, _, _) ->
    Other.

accept_(RequestContext, Token) ->
    {ok, [{?CSRFTOKEN_NAME, Token} | RequestContext]}.

reject_(Reason) ->
    {error, {csrf, Reason}}.


csrftoken_name() ->
    %% Returns CSRFTOKEN_NAME
    ?CSRFTOKEN_PARAM_NAME.

%%%%%%%%%%%%%%%%%
%% Internal stuff
%%%%%%%%%%%%%%%%%

new_token() ->
    get_random_string(12).

pre_check_csrf_token(RequestContext, CSRF_Token, NewToken) ->
    Request = proplists:get_value(request, RequestContext),
    %% Pre check CSRF token
    case CSRF_Token =:= none of
        true ->
            reject_(no_csrf_cookie);
        false ->
            case Request:post_param(?CSRFTOKEN_PARAM_NAME) of
                undefined ->
                    %% No CSRF Token in POST, let's check HEADERS
                    case proplists:get_value(?CSRFTOKEN_HEADER, Request:headers()) of
                        undefined -> reject_(no_csrf_token);
                        HTTPToken ->
                            check_csrf_token(RequestContext, HTTPToken, CSRF_Token, NewToken)
                    end;
                PostToken ->
                    %% We have token, let's check it
                    check_csrf_token(RequestContext, PostToken, CSRF_Token, NewToken)
            end
    end.

check_csrf_token(RequestContext, PostToken, CSRF_Token, NewToken) ->
    %% Check if token provided in request is same as we have in cookies
    case PostToken =:= CSRF_Token of
        true ->
            accept_(RequestContext, NewToken);
        false ->
            reject_(incorrect_csrf_token)
    end.



get_csrf_token(Request) ->
    %% Get CSRF Token from cookies
    case Request:cookie(atom_to_list(?CSRFTOKEN_NAME)) of
        "undefined" ->
            %% Generate new Token
            [none, new_token()];
        undefined ->
            %% Generate new Token
            [none, new_token()];
        Token ->
            [Token, Token]
    end.

get_random_string(Length) ->
    %% Generates random string, used as CSRF Token
    get_random_string(Length, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

check_referer(Req) ->
    %% Check referer, if needed
    case Req:protocol() of
        http ->
            true;
        Protocol ->
            %% Check referer, only on secure requests
            case proplists:get_value(referer, Req:headers()) of
                undefined ->
                    false;
                _Referer -> %% TODO: Finish referer check
                    same_host(Req:header("referer"), Protocol, Req:header("host"))
            end
    end.

same_host(Referer, Protocol, Host) ->
    ProtocolHost = string:join([atom_to_list(Protocol), Host], "://"),
    CutReferer = string:substr(Referer, 1, string:len(ProtocolHost)),
    string:equal(ProtocolHost, CutReferer).

template_field(Token) ->
    io_lib:format("<input type=\"hidden\" value=\"~s\" name=\"~s\" />", [Token, ?CSRFTOKEN_PARAM_NAME]).

