
-define(DEBUGPRINT(A), error_logger:info_report("~~o)> " ++ A)).
-define(BUILTIN_CONTROLLER_FILTERS, [boss_lang_filter, boss_cache_page_filter, boss_cache_vars_filter]).
-define(DEFAULT_WEB_SERVER, cowboy).
-define(PRINT(N,V),
	io:format("~s:~p (~p) ~s ~p~n", [?FILE, ?LINE, self(), N,V])).
-record(state, {
        applications	= []	::[atom()],
        service_sup_pid		::pid(),
        http_pid		::pid(),
        smtp_pid		::pid(),
        is_master_node	= false ::boolean()
    }).

-record(boss_app_info, {
        application		::atom(),
        base_url,
        static_prefix,
        doc_prefix,
        domains,
        init_data,
        router_sup_pid		::pid(),
        router_pid		::pid(),
        translator_sup_pid	::pid(),
        translator_pid		::pid(),
        model_modules = [],
        view_modules = [],
        controller_modules = []
    }).

