
-define(DEBUGPRINT(A), error_logger:info_report("~~o)> " ++ A)).
-define(BUILTIN_CONTROLLER_FILTERS, [boss_lang_filter, boss_cache_page_filter, boss_cache_vars_filter]).
-define(DEFAULT_WEB_SERVER, cowboy).
-define(PRINT(N,V),
	lager:notice(" ~s ~p", [ N,V])).
-include_lib("eunit/include/eunit.hrl").

-record(boss_app_info, {
        application		::atom(),
        base_url                ::string(),
        static_prefix           ::string(),
        doc_prefix              ::string(),
        domains                 ::all|[string()],
        init_data               ::[{atom(), any()}] ,
        router_sup_pid		::pid(),
        router_pid		::pid(),
        translator_sup_pid	::pid(),
        translator_pid		::pid(),
        model_modules = []      ::[atom()],
        view_modules = []       ::[atom()],
        controller_modules = [] ::[atom()]
    }).

-record(state, {
        applications	= []	::[ #boss_app_info{}],
        router_adapter          ::atom(),
        service_sup_pid		::pid(),
        http_pid		::pid(),
        smtp_pid		::pid(),
        is_master_node	= false ::boolean()
    }).


-ifdef(TEST).
-compile(export_all).
-endif.
