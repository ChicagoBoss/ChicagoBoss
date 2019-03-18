%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc
%%-------------------------------------------------------------------

-module(boss_cowboy_dispacher).

-export([build_and_compile/0]).

%% cowboy dispatch rule for static content
build_and_compile() ->
    Applications        = boss_env:get_env(applications, []),
    AppStaticDispatches = create_cowboy_dispatches(Applications),
    BossDispatch        = [{'_', cowboy_simple_bridge_anchor,[]}],
    Dispatch            = [{'_', AppStaticDispatches ++ BossDispatch}],

    cowboy_router:compile(Dispatch).

create_cowboy_dispatches(Applications) ->
    lists:map(fun create_dispatch/1, Applications).

-spec(create_dispatch(atom()) -> {[any(),...], cowboy_static, {'priv_dir',atom(),[97 | 99 | 105 | 115 | 116,...],[{_,_,_},...]}}).
create_dispatch(AppName) ->
    application:load(AppName),
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
            _ = lager:warning("Unable to determine code:priv_dir for app ~p. The most common cause of this is your ChicagoBoss app having a different name than the directory it's in. Using ./priv/", [AppName]),
            {dir, "./priv/static", Extra};
        Priv ->
            case filelib:is_dir(Priv) of
                true ->
                    {priv_dir, AppName, "static", Extra};
                false ->
                    _ = lager:warning("~p's priv_dir ~p was not found. Using ./priv/ as a fallback.", [AppName, Priv]),
                    {dir, "./priv/static", Extra}
            end
    end.
