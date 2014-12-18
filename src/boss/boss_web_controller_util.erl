-module(boss_web_controller_util).

-export([start_boss_applications/3]).
-export([execution_mode/1]).

-include("boss_web.hrl").

-spec execution_mode(types:application()) -> types:execution_mode().

start_boss_applications( Applications, ServicesSupPid, #state{router_adapter=RouterAdapter}) ->
    lists:map(
        fun(AppName) ->
            application:start(AppName),
            BaseURL                = boss_env:get_env(AppName, base_url, "/"),
            StaticPrefix           = boss_env:get_env(AppName, static_prefix, "/static"),
            DocPrefix              = boss_env:get_env(AppName, doc_prefix, "/doc"),
            DomainList             = boss_env:get_env(AppName, domains, all),
            ModelList              = boss_files:model_list(AppName),
            ViewList               = boss_files:view_module_list(AppName),
            IsMasterNode           = boss_env:is_master_node(),
            ControllerList         = case boss_files:web_controller_list(AppName) of
                                        [] -> case boss_env:is_developing_app(AppName) of
                                                true  -> [];
                                                false -> 
                                                    lager:warning("App ~p doesn't seem to have controllers defined,~n"
                                                                  "check your config file at section <~p:controller_modules>,~n"
                                                                  "have you compiled your CB application?",[AppName, AppName]), 
                                                    []
                                              end;
                                        List -> List
                                     end,

            {ok, RouterSupPid}     = RouterAdapter:start([{application, AppName},
                                                          {controllers, ControllerList}]),
            {ok, TranslatorSupPid} = boss_translator:start([{application, AppName}]),
            init_app_load_on_dev(AppName, TranslatorSupPid),

            enable_master_apps(ServicesSupPid, 
                               AppName, 
                               BaseURL,
                               IsMasterNode),
            
            InitData = boss_web_controller:run_init_scripts(AppName),

            #boss_app_info
            {
                application         = AppName,
                init_data           = InitData,
                router_sup_pid      = RouterSupPid,
                translator_sup_pid  = TranslatorSupPid,
                base_url            = if BaseURL =:= "/" -> ""; true -> BaseURL end,
                static_prefix       = StaticPrefix,
                doc_prefix          = DocPrefix,
                domains             = DomainList,
                model_modules       = ModelList,
                view_modules        = ViewList,
                controller_modules  = ControllerList
            }
        end, Applications).

init_app_load_on_dev(AppName, TranslatorSupPid) ->
    case boss_env:is_developing_app(AppName) of
    true  -> 
            Result = boss_load:load_all_modules(AppName, TranslatorSupPid),
            Result;
    false -> ok
    end.

enable_master_apps(ServicesSupPid, AppName, BaseURL, IsMasterNode) ->
    if
        IsMasterNode ->
            case boss_env:get_env(server, ?DEFAULT_WEB_SERVER) of
                cowboy ->
                    WebSocketModules = boss_files:websocket_list(AppName),
                    MappingServices  = boss_files:websocket_mapping(BaseURL,
                        atom_to_list(AppName),
                        WebSocketModules),
                    boss_service_sup:start_services(ServicesSupPid, MappingServices);
                _Any ->
                    _Any
            end;
        true ->
            ok
    end.


execution_mode(App) ->
    case boss_env:is_developing_app(App) of
	true  -> development;
	false -> production
    end.
