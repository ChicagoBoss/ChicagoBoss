-module(boss_web_controller_util).

-export([make_boss_app_info/10]).

-export([unpack_application_env/1]).

-export([find_application_for_path/3]).

-export([execution_mode/1]).

-include("boss_web.hrl").

-spec execution_mode(types:application()) -> types:execution_mode().
-spec find_application_for_path('undefined' | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []),_,[any()]) -> any().
-spec make_boss_app_info(types:application(),_,_,_,_,_,_,_,_,_) -> #boss_app_info{}.
-spec unpack_application_env(types:application()) -> {_,_,_,_,_,_,_,_,_,_}.

make_boss_app_info(AppName, BaseURL, StaticPrefix, DocPrefix,
                   DomainList, ModelList, ViewList, ControllerList,
		   RouterSupPid, TranslatorSupPid) ->
    InitData = boss_web_controller:run_init_scripts(AppName),
    #boss_app_info{
		    application         = AppName,
		    init_data           = InitData,
		    router_sup_pid      = RouterSupPid,
		    translator_sup_pid  = TranslatorSupPid,
		    base_url            =  if BaseURL =:= "/" -> ""; true -> BaseURL end,
		    static_prefix       = StaticPrefix,
		    doc_prefix          = DocPrefix,
		    domains             = DomainList,
                    model_modules       = ModelList,
                    view_modules        = ViewList,
                    controller_modules  = ControllerList
    }.

unpack_application_env( AppName) ->
    BaseURL                     = boss_env:get_env(AppName, base_url, "/"),
    StaticPrefix                = boss_env:get_env(AppName, static_prefix, "/static"),
    DocPrefix			= boss_env:get_env(AppName, doc_prefix, "/doc"),
    DomainList			= boss_env:get_env(AppName, domains, all),
    ModelList			= boss_files:model_list(AppName),
    ViewList			= boss_files:view_module_list(AppName),
    IsMasterNode                = boss_env:is_master_node(),
    ControllerList              = boss_files:web_controller_list(AppName),
    {ok, RouterSupPid}		= boss_router:start([{application, AppName},
						     {controllers, ControllerList}]),
    {ok, TranslatorSupPid}	= boss_translator:start([{application, AppName}]),
    {TranslatorSupPid, BaseURL, IsMasterNode, StaticPrefix, DocPrefix,
     DomainList, ModelList, ViewList, ControllerList, RouterSupPid}.

find_application_for_path(Host, Path, Applications) ->
    UseHost = case Host of
        undefined -> undefined;
        _ -> hd(re:split(Host, ":", [{return, list}]))
    end,
    find_application_for_path(UseHost, Path, undefined, Applications, -1).

find_application_for_path(_Host, _Path, Default, [], _MatchScore) ->
    Default;
find_application_for_path(Host, Path, Default, [App|Rest], MatchScore) ->
    DomainScore = case Host of
        undefined -> 0;
        _ ->
            case boss_web:domains(App) of
                all -> 0;
                Domains ->
                    case lists:member(Host, Domains) of
                        true -> 1;
                        false -> -1
                    end
            end
    end,
    BaseURL = boss_web:base_url(App),
    PathScore = length(BaseURL),
    {UseApp, UseScore} = case (DomainScore >= 0) andalso (1000 * DomainScore + PathScore > MatchScore) andalso lists:prefix(BaseURL, Path) of
        true -> {App, DomainScore * 1000 + PathScore};
        false -> {Default, MatchScore}
    end,
    find_application_for_path(Host, Path, UseApp, Rest, UseScore).



execution_mode(App) ->
    case boss_env:is_developing_app(App) of
	true  -> development;
	false -> production
    end.
