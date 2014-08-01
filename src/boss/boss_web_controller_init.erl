-module(boss_web_controller_init).

-export([init_lager/0]).

-export([init_cache/0]).

-export([init_master_node/2]).

-export([init_webserver/7]).

-export([init_ssl/0]).

-export([init_master_services/2]).

-export([init_mail_service/0]).

-export([init_services/0]).
-type cb_node() :: types:cb_node().
-spec init_cache() -> 'ignore' | 'ok' | {'error',_} | {'ok',pid()}.
-spec init_lager() -> 'ok'.
-spec init_mail_service() -> any().
-spec init_master_node(_,cb_node()) -> {'ok',_}.
-spec init_master_services(cb_node(),cb_node()) -> any().
-spec init_ssl() -> {boolean(),_}.
-spec init_webserver(cb_node,cb_node(),types:webserver(),boolean(),_,pid()|undefined,_) -> any().

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
                        MaxSize		= boss_env:get_env(ets_maxsize, 32 * 1024 * 1024),
                        Threshold	= boss_env:get_env(ets_threshold, 0.85),
                        Weight		= boss_env:get_env(ets_weight, 30),
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
            error_logger:info_msg("Starting master services on ~p~n", [MasterNode]),
            boss_mq:start(),
            boss_news:start(),
            case boss_env:get_env(smtp_server_enable, false) of
                true ->
                    Options = [
                        {domain,	boss_env:get_env(smtp_server_domain, "localhost")},
                        {address,	boss_env:get_env(smtp_server_address, {0, 0, 0, 0})},
                        {port,		boss_env:get_env(smtp_server_port, 25)},
                        {protocol,	boss_env:get_env(smtp_server_protocol, tcp)},
                        {sessionoptions, [{boss_env, Env}]}],
                    gen_smtp_server:start({global, boss_smtp_server}, boss_smtp_server, [Options]);
                _ ->
                    ok
            end;
        true ->
            error_logger:info_msg("Pinging master node ~p from ~p~n", [MasterNode, ThisNode]),
            pong = net_adm:ping(MasterNode)
    end,
    {ok,MasterNode}.


init_webserver(ThisNode, MasterNode, ServerMod, SSLEnable, SSLOptions,
               ServicesSupPid, ServerConfig) ->
    case ServerMod of
  mochiweb_http ->
      application:start(mochiweb),
      mochiweb_http:start([{ssl, SSLEnable}, {ssl_opts, SSLOptions} | ServerConfig]);
  cowboy ->
	    %Dispatch = [{'_', [{'_', boss_mochicow_handler, [{loop, {boss_mochicow_handler, loop}}]}]}],
	    error_logger:info_msg("Starting cowboy... on ~p~n", [MasterNode]),
	    application:start(cowlib),
	    application:start(ranch),
        application:start(cowboy),
        HttpPort      = boss_env:get_env(port, 8001),
        HttpIp        = boss_env:get_env(ip, {0, 0, 0, 0}),
        AcceptorCount = boss_env:get_env(acceptor_processes, 100),
        case SSLEnable of
            false ->
                error_logger:info_msg("Starting http listener... on ~s:~p ~n", [inet_parse:ntoa(HttpIp), HttpPort]),
                cowboy:start_http(boss_http_listener, AcceptorCount, [{port, HttpPort}, {ip, HttpIp}], [{env, []}]);
            true ->
                error_logger:info_msg("Starting https listener... on ~s:~p ~n", [inet_parse:ntoa(HttpIp), HttpPort]),
        	    SSLConfig = [{port, HttpPort}, {ip, HttpIp}] ++ SSLOptions,
        	    cowboy:start_https(boss_https_listener, AcceptorCount, SSLConfig, [{env, []}])
        end,
        if MasterNode =:= ThisNode ->
                boss_service_sup:start_services(ServicesSupPid, boss_websocket_router);
	       true -> ok
        end
    end.


init_ssl() ->
    SSLEnable  = boss_env:get_env(ssl_enable, false),
    SSLOptions = boss_env:get_env(ssl_options, []),
    error_logger:info_msg("SSL:~p~n", [SSLOptions]),
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
    error_logger:info_msg("Starting Boss in ~p mode....~n", [Env]),
    boss_model_manager:start(),
    init_cache(),
    boss_session:start(),
    Env.
