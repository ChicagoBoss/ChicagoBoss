-module(boss_mail_driver_smtp).
-export([start/0, stop/1, deliver/4]).

start() ->
    OptionsBase = [{auth, never}, {tls, if_available},
        {ssl, false}, {hostname, smtp_util:guess_FQDN()}, {retries, 1}],
    Options = case application:get_env(mail_relay) of
        {ok, Relay} ->
            [{relay, Relay} | OptionsBase];
        undefined ->
            OptionsBase
    end,
    {ok, Options}.

stop(_) ->
    ok.

deliver(Options, FromAddress, ToAddress, BodyFun) ->
    MailOptions = case proplists:lookup(relay, Options) of
        none ->
            [_User, Host] = string:tokens(ToAddress, "@"),
            [{relay, Host} | Options];
        _ ->
            Options
    end,
    Email = {FromAddress, [ToAddress], BodyFun},
    gen_smtp_client:send(Email, MailOptions).
