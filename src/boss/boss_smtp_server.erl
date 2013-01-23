-module(boss_smtp_server).
-behaviour(gen_smtp_server_session).

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
        handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
        handle_other/3, code_change/3, terminate/2]).

-record(state, {
        remote_ip,
        errors,
        authorized_apps = []
    }).

init(Hostname, _SessionCount, Address, Options) ->
    Errors = case proplists:get_value(boss_env, Options, development) of
        development -> 
            Application = boss_env:get_env(developing_app, undefined),
            case boss_load:load_mail_controllers(Application) of
                {ok, _} -> 
                    case boss_load:load_libraries(Application) of
                        {ok, _} ->
                            case boss_load:load_models(Application) of
                                {ok, _} -> [];
                                {error, List} -> List
                            end;
                        {error, List} -> List
                    end;
                {error, List} -> List
            end;
        _ -> []
    end,
    {ok, [Hostname, " SMTP Chicago Boss: Tell me something I don't know"], 
        #state{remote_ip = Address, errors = Errors}}. 

handle_HELO(_Hostname, State) ->
    {ok, 655360, State}.

handle_EHLO(_Hostname, Extensions, State) ->
    {ok, Extensions, State}.

handle_MAIL(FromAddress, State) ->
    case State#state.errors of
        [] ->
            [UserName, DomainName] = re:split(FromAddress, "@"),
            Authorize = lists:foldl(fun
                    (App, Acc) ->
                        Controller = boss_load:incoming_mail_controller_module(App),
                        lists:member({'authorize_', 3}, Controller:module_info(exports)) andalso
                        case Controller:authorize_(binary_to_list(UserName), binary_to_list(DomainName), State#state.remote_ip) of
                            true ->
                                [App|Acc];
                            false ->
                                Acc
                        end
                end, [], boss_web:get_all_applications()),
            case Authorize of
                [] -> {error, "552 go away", State};
                _ -> {ok, State#state{ authorized_apps = Authorize }}
            end;
        _Errors ->
            {ok, State}
    end.

handle_MAIL_extension(_Extension, _State) ->
    error.

handle_RCPT(ToAddress, State) ->
    case State#state.errors of
        [] ->
            [UserName, _DomainName] = re:split(ToAddress, "@"),
            RecipientExists = lists:foldl(fun
                    (_, true) -> true;
                    (App, false) ->
                        Controller = boss_load:incoming_mail_controller_module(App),
                        ExportStrings = lists:map(
                            fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
                            Controller:module_info(exports)),
                        lists:member({binary_to_list(UserName), 2}, ExportStrings)
                end, false, State#state.authorized_apps),
            case RecipientExists of
                true ->
                    {ok, State};
                false ->
                    {error, "550 No such recipient", State}
            end;
        _Errors ->
            {ok, State}
    end.

handle_RCPT_extension(_Extension, _State) ->
    error.

handle_DATA(FromAddress, ToAddressList, Data, #state{ errors = [] } = State) ->
    Errors = try mimemail:decode(Data) of
        Message ->
            lists:foldl(fun
                    (ToAddress, ErrorAcc) ->
                        [UserName, _DomainName] = re:split(ToAddress, "@"),
                        {Res1, done} = lists:foldl(fun
                                (_, {Res, done}) -> {Res, done};
                                (App, _) ->
                                    Controller = boss_load:incoming_mail_controller_module(App),
                                    ExportStrings = lists:map(
                                        fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
                                        Controller:module_info(exports)),
                                    case lists:member({binary_to_list(UserName), 2}, ExportStrings) of
                                        true ->
                                            Function = list_to_atom(binary_to_list(UserName)),
                                            {Controller:Function(binary_to_list(FromAddress), Message), done};
                                        false ->
                                            {undefined, not_done}
                                    end
                            end, {undefined, not_done}, State#state.authorized_apps),
                        case Res1 of
                            ok -> ErrorAcc;
                            {error, Error} -> [Error|ErrorAcc]
                        end
                end, [], ToAddressList)
        catch
            What:Why ->
                [io_lib:format("Message decode FAILED with ~p:~p~n", [What, Why])]
        end,

    reply_with_errors(FromAddress, ToAddressList, State#state{ errors = Errors });
handle_DATA(FromAddress, ToAddressList, _Data, State) ->
    reply_with_errors(FromAddress, ToAddressList, State).

reply_with_errors(_FromAddress, _ToAddressList, #state{ errors = [] } = State) ->
    Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),
    {ok, Reference, State};
reply_with_errors(FromAddress, ToAddressList, #state{ errors = Errors } = State) ->
    FirstAddress = hd(ToAddressList),
    boss_mail:send(binary_to_list(FirstAddress), binary_to_list(FromAddress), 
        "ERROR", "There were errors delivering your message: ~p~n", [Errors]),
    reply_with_errors(FromAddress, ToAddressList, State#state{ errors = [] }).

handle_RSET(State) ->
    State.

handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.

handle_other(Verb, _Args, State) ->
    {["500 Error: command not recognized : '", Verb, "'"], State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    {ok, Reason, State}.
