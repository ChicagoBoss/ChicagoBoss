-module(boss_mail_driver_smtp_direct).
-export([start/0, stop/0, deliver/3]).

start() ->
    Options = [{auth, never}, {tls, if_available}, 
        {ssl, false}, {hostname, smtp_util:guess_FQDN()}, {retries, 1}],
    register(boss_mail_smtp_direct, spawn(fun() -> loop(Options) end)),
    ok.

stop() ->
    ok.

loop(Options) ->
    receive
        {From, {deliver, FromAddress, ToAddress, BodyFun}} ->
            [_User, Host] = string:tokens(ToAddress, "@"),
            Email = {FromAddress, [ToAddress], BodyFun},
            gen_smtp_client:send(Email, [{relay, Host}|Options]),
            From ! {boss_mail_smtp_direct, ok},
            loop(Options)
    end.

deliver(FromAddress, ToAddress, BodyFun) ->
    boss_mail_smtp_direct ! {self(), {deliver, FromAddress, ToAddress, BodyFun}},
    receive
        {boss_mail_smtp_direct, ok} ->
            ok
    end.
