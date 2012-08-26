-module(boss_web).

-export([reload_routes/0,
        reload_translation/1,
        reload_all_translations/0,
        reload_init_scripts/0,
        get_all_routes/0,
        get_all_models/0,
        get_all_applications/0,
        base_url/1,
        domains/1,
        static_prefix/1,
        translator_pid/1,
        router_pid/1,
        application_info/1]).

reload_routes() ->
    gen_server:call(boss_web, reload_routes).

reload_translation(Locale) ->
    gen_server:call(boss_web, {reload_translation, Locale}).

reload_all_translations() ->
    gen_server:call(boss_web, reload_all_translations).

reload_init_scripts() ->
    gen_server:call(boss_web, reload_init_scripts).

get_all_routes() ->
    gen_server:call(boss_web, get_all_routes).

get_all_models() ->
    gen_server:call(boss_web, get_all_models).

get_all_applications() ->
    gen_server:call(boss_web, get_all_applications).

base_url(App) ->
    gen_server:call(boss_web, {base_url, App}).

domains(App) ->
    gen_server:call(boss_web, {domains, App}).

static_prefix(App) ->
    gen_server:call(boss_web, {static_prefix, App}).

translator_pid(AppName) ->
    gen_server:call(boss_web, {translator_pid, AppName}).

router_pid(AppName) ->
    gen_server:call(boss_web, {router_pid, AppName}).

application_info(App) ->
    gen_server:call(boss_web, {application_info, App}).
