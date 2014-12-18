-module(boss_web_controller_cowboy).

-export([dispatch_cowboy/1]).

%% cowboy dispatch rule for static content
dispatch_cowboy(Applications) ->
    AppStaticDispatches = create_cowboy_dispatches(Applications),
    RouterAdapter       = boss_env:router_adapter(),
    BossDispatch	= [{'_', boss_mochicow_handler, [{loop, {boss_mochicow_handler, loop, [RouterAdapter]}}]}],
    % [{"/", boss_mochicow_handler, []}],
    %Dispatch		= [{'_',

    Dispatch		= [{'_', AppStaticDispatches ++ BossDispatch}],
    SSLEnabled = boss_env:get_env(ssl_enable, false),
    CowboyListener        = get_listener(SSLEnabled),
    cowboy:set_env(CowboyListener, dispatch, cowboy_router:compile(Dispatch)).

-spec(get_listener(boolean()) -> boss_https_listener|boss_http_listener).
get_listener(true) -> boss_https_listener;
get_listener(false) -> boss_http_listener.

create_cowboy_dispatches(Applications) ->
    lists:map(fun create_dispatch/1, Applications).

-spec(create_dispatch(atom()) -> {[any(),...], cowboy_static, {'priv_dir',atom(),[97 | 99 | 105 | 115 | 116,...],[{_,_,_},...]}}).
create_dispatch(AppName) ->
    BaseURL             = boss_env:get_env(AppName, base_url, "/"),
    StaticPrefix        = boss_env:get_env(AppName, static_prefix, "/static"),
    Path                = case BaseURL of
                              "/" -> StaticPrefix;
                              _ -> BaseURL ++ StaticPrefix
                          end,
    Handler             = cowboy_static,
    Etag                = [], %%[{etag, false}], %% [{etag, EtagModule, EtagFunction}]
    MimeTypes           = [{mimetypes, cow_mimetypes, all}], %% [{mimetypes, mimetypes, path_to_mimes}]                          
    Extra               = Etag ++ MimeTypes,
    Opts                = create_cowboy_static_opts(AppName, Extra),
    {Path ++ "/[...]", Handler, Opts}.

create_cowboy_static_opts(AppName, Extra) ->
    case code:priv_dir(AppName) of
        {error, bad_name} ->
            lager:warning("Unable to determine code:priv_dir for app ~p. The most common cause of this is your ChicagoBoss app having a different name than the directory it's in. Using ./priv/", [AppName]),
            {dir, "./priv/static", Extra};
        Priv ->
            case filelib:is_dir(Priv) of
                true ->
                    {priv_dir, AppName, "static", Extra};
                false ->
                    lager:warning("~p's priv_dir ~p was not found. Using ./priv/ as a fallback.", [AppName, Priv]),
                    {dir, "./priv/static", Extra}
            end
    end.
