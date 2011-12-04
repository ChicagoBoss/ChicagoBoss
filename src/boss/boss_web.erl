-module(boss_web).

-compile(export_all).

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

translator_pid(AppName) ->
    gen_server:call(boss_web, {translator_pid, AppName}).

router_pid(AppName) ->
    gen_server:call(boss_web, {router_pid, AppName}).

application_info(App) ->
    gen_server:call(boss_web, {application_info, App}).
