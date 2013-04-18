
-record(boss_app_info, {
        application,
        base_url,
        static_prefix,
        doc_prefix,
        domains,
        init_data,
        router_sup_pid,
        router_pid,
        translator_sup_pid,
        translator_pid,
        model_modules = [],
        view_modules = [],
        controller_modules = []
    }).

