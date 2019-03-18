
-define(DEBUGPRINT(A), error_logger:info_report("~~o)> " ++ A)).
-define(BUILTIN_CONTROLLER_FILTERS, [boss_lang_filter, boss_cache_page_filter, boss_cache_vars_filter]).
-define(PRINT(N,V),
    lager:notice(" ~s ~p", [ N,V])).
-include_lib("eunit/include/eunit.hrl").

-record(boss_app_info, {
        application             :: atom(),
        base_url                :: 'undefined' | string(),
        static_prefix           :: 'undefined' | string(),
        doc_prefix              :: 'undefined' | string(),
        domains                 :: 'undefined' | 'all' | [string()],
        init_data               :: 'undefined' | [{atom(), any()}] ,
        router_sup_pid          :: 'undefined' | pid(),
        router_pid              :: 'undefined' | pid(),
        translator_sup_pid      :: 'undefined' | pid(),
        translator_pid          :: 'undefined' | pid(),
        model_modules = []      :: [atom()],
        view_modules = []       :: [atom()],
        controller_modules = [] :: [atom()]
    }).

-record(state, {
        applications    = []    :: [ #boss_app_info{}],
        router_adapter          :: atom(),
        service_sup_pid         :: pid(),
        http_pid                :: pid(),
        smtp_pid                :: pid(),
        is_master_node  = false :: boolean()
    }).


-ifdef(TEST).
-compile(export_all).
-endif.
