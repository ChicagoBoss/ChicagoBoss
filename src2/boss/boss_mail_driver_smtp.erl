-module(boss_mail_driver_smtp).
-export([start/0, stop/0, deliver/3]).

start() ->
     OptionsBase = [{auth, never}, {tls, if_available},
            {ssl, false}, {hostname, smtp_util:guess_FQDN()}, {retries, 1}],
     Options = case application:get_env(mail_relay) of
           {ok, Relay} ->
               [{relay, Relay} | OptionsBase];
           undefined ->
               OptionsBase
           end,
     register(boss_mail_smtp, spawn(fun() -> loop(Options, dict:new()) end)),
    ok.

stop() ->
    ok.

loop(Options, PidDict) ->
    receive
        {'EXIT', From, normal} ->
            loop(Options, dict:erase(From, PidDict));
        {'EXIT', From, Reason} ->
            {FromAddress, [ToAddress], BodyFun} = dict:fetch(From, PidDict),
            EmailFailure = boss_mail_failure:new(id, erlang:now(), FromAddress, ToAddress, 
                term_to_binary(BodyFun), term_to_binary(Reason)),
            EmailFailure:save(),
            loop(Options, dict:erase(From, PidDict));
        {From, {deliver, FromAddress, ToAddress, BodyFun}} ->
            MailOptions = case proplists:lookup(relay, Options) of
                none ->
                    [_User, Host] = string:tokens(ToAddress, "@"),
                    [{relay, Host} | Options];
                _ ->
                    Options
            end,
            Email = {FromAddress, [ToAddress], BodyFun},
            {RetVal, NewDict} = case gen_smtp_client:send(Email, MailOptions) of
                {ok, SenderPid} ->
                    {ok, dict:store(SenderPid, Email, PidDict)};
                {error, Reason} ->
                    {{error, Reason}, PidDict}
            end,
            From ! {boss_mail_smtp, RetVal},
            loop(Options, NewDict)
    end.

deliver(FromAddress, ToAddress, BodyFun) ->
    boss_mail_smtp ! {self(), {deliver, FromAddress, ToAddress, BodyFun}},
    receive
        {boss_mail_smtp, ok} ->
            ok
    end.
