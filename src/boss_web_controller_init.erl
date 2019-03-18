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

-module(boss_web_controller_init).

-export([init_lager/0]).

-export([init_cache/0]).

-export([init_master_node/2]).

-export([init_webserver/6]).

-export([init_ssl/0]).

-export([init_master_services/2]).

-export([init_mail_service/0]).

-export([init_services/0]).
-type cb_node() :: types:cb_node().
-spec init_cache() -> 'ignore' | 'ok' | {'error',_} | {'ok',pid()}.
-spec init_lager() -> 'ok'.
-spec init_mail_service() -> 'ignore' | {'error',_} | {'ok',pid()}.
-spec init_master_node(_,cb_node()) -> {'ok',_}.
-spec init_master_services(cb_node(),cb_node()) -> any().
-spec init_ssl() -> {boolean(),_}.
-spec init_webserver(cb_node,cb_node(),boolean(),_,pid()|undefined,_) -> any().

init_lager() ->
    case boss_env:get_env(log_enable, true) of
        false -> ok;
        true  -> lager:start()
    end.

init_cache() ->
    case boss_env:get_env(cache_enable, false) of
        false -> ok;
        true  ->
            CacheAdapter = boss_env:cache_adapter(),
            CacheOptions =
                case CacheAdapter of
                    ets ->
                        MaxSize        = boss_env:get_env(ets_maxsize, 32 * 1024 * 1024),
                        Threshold    = boss_env:get_env(ets_threshold, 0.85),
                        Weight        = boss_env:get_env(ets_weight, 30),
                        [{adapter, ets},
             {ets_maxsize, MaxSize},
                         {ets_threshold, Threshold},
             {ets_weight, Weight}];
                    _ ->
                        [{adapter, CacheAdapter},
                         {cache_servers, boss_env:get_env(cache_servers, [{"127.0.0.1", 11211, 1}])}]
                end,
            boss_cache:start(CacheOptions)
    end.


init_master_node(Env, ThisNode) ->
    MasterNode = boss_env:master_node(),
    if
        MasterNode =:= ThisNode ->
            _ = lager:debug("Starting master services on ~p", [MasterNode]),
            _ = boss_mq:start(),
            _ = boss_news:start(),
            case boss_env:get_env(smtp_server_enable, false) of
                true ->
                    Options = [
                        {domain,    boss_env:get_env(smtp_server_domain, "localhost")},
                        {address,    boss_env:get_env(smtp_server_address, {0, 0, 0, 0})},
                        {port,        boss_env:get_env(smtp_server_port, 25)},
                        {protocol,    boss_env:get_env(smtp_server_protocol, tcp)},
                        {sessionoptions, [{boss_env, Env}]}],
                    gen_smtp_server:start({global, boss_smtp_server}, boss_smtp_server, [Options]);
                _ ->
                    ok
            end;
        true ->
            _ = lager:debug("Pinging master node ~p from ~p", [MasterNode, ThisNode]),
            pong = net_adm:ping(MasterNode)
    end,
    {ok,MasterNode}.


init_webserver(ThisNode, MasterNode, SSLEnable, SSLOptions,
               ServicesSupPid, ServerConfig) ->

        _ = lager:debug("Starting webserver... on ~p", [MasterNode]),
        application:start(simple_bridge),

        if MasterNode =:= ThisNode ->
                boss_service_sup:start_services(ServicesSupPid, boss_websocket_router);
           true -> ok
        end.


init_ssl() ->
    SSLEnable  = boss_env:get_env(ssl_enable, false),
    SSLOptions = boss_env:get_env(ssl_options, []),
    {SSLEnable, SSLOptions}.


init_master_services(ThisNode, MasterNode) ->
    {ok, ServicesSupPid}  = case MasterNode of
                                ThisNode ->
                                    boss_service_sup:start_link();
                                _AnyNode ->
                                    {ok, undefined}
                            end,
    ServicesSupPid.


init_mail_service() ->
    MailDriver = boss_env:get_env(mail_driver, boss_mail_driver_smtp),
    boss_mail:start([{driver, MailDriver}]).


init_services() ->
    init_lager(),
    application:start(elixir),

    Env = boss_env:setup_boss_env(),
    _ = lager:info("Starting Boss in ~p mode...", [Env]),
    boss_model_manager:start(),
    init_cache(),
    boss_session:start(),
    Env.
