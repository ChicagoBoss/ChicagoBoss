-module(boss_mail_driver_smtp_direct).
-export([start/0, stop/0, deliver/3]).

start() ->
    Options = [{auth, never}, {tls, if_available}, 
        {ssl, false}, {hostname, smtp_util:guess_FQDN()}, {retries, 1}],
    register(boss_mail_smtp_direct, spawn(fun() -> loop(Options, dict:new()) end)),
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
            [_User, Host] = string:tokens(ToAddress, "@"),
            Email = {FromAddress, [ToAddress], BodyFun},
            {RetVal, NewDict} = case gen_smtp_client:send(Email, [{relay, Host}|Options]) of
                {ok, SenderPid} ->
                    {ok, dict:store(SenderPid, Email, PidDict)};
                {error, Reason} ->
                    {{error, Reason}, PidDict}
            end,
            From ! {boss_mail_smtp_direct, RetVal},
            loop(Options, NewDict)
    end.

deliver(FromAddress, ToAddress, BodyFun) ->
    boss_mail_smtp_direct ! {self(), {deliver, FromAddress, ToAddress, BodyFun}},
    receive
        {boss_mail_smtp_direct, ok} ->
            ok
    end.
