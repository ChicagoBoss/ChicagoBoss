-module(boss_mail_driver_smtp).
-export([start/0, stop/1, deliver/5]).

start() ->
    OptionsBase = [{ssl, false}, {hostname, smtp_util:guess_FQDN()}, {retries, 1}],
    {ok, OptionsBase ++ get_tls() ++ get_host() ++ get_port() ++ get_credentials()}.

get_tls() ->
    case application:get_env(mail_relay_use_tls) of
        {ok, Setting} ->
            [{tls, Setting}];
        undefined ->
            [{tls, if_available}]
    end.

get_host() ->
    case application:get_env(mail_relay_host) of
        {ok, Relay} ->
            [{relay, Relay}];
        undefined ->
            []
    end.

get_port() ->
    case application:get_env(mail_relay_port) of
        {ok, Port} ->
            [{port, Port}];
        undefined ->
            []
    end.

get_credentials() ->
    case application:get_env(mail_relay_username) of
        {ok, UserName} ->
            [{auth, always}, {username, UserName}, {password, boss_env:get_env(mail_relay_password, "")}];
        _ ->
            [{auth, never}]
    end.

stop(_) ->
    ok.

deliver(Options, FromAddress, ToAddress, BodyFun, ResultFun) ->
    MailOptions = case proplists:lookup(relay, Options) of
        none ->
            [_User, Host] = string:tokens(ToAddress, "@"),
            [{relay, Host} | Options];
        _ ->
            Options
    end,
    Email = {FromAddress, [ToAddress], BodyFun},
    gen_smtp_client:send(Email, MailOptions, ResultFun).
