
-record(boss_app_info, {
        application,
        base_url,
        watches,
        router_sup_pid,
        router_pid,
        translator_sup_pid,
        translator_pid,
        model_modules = [],
        controller_modules = []
    }).

