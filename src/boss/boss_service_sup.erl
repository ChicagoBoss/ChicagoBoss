%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@monolite>
%%% @copyright (C) 2012, mihawk
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2012 by mihawk <mihawk@monolite>
%%%-------------------------------------------------------------------
-module(boss_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_services/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------

start_services(SupPid, boss_websocket_router) ->
    {ok, BossWebSocketRouterPid} = 
	supervisor:start_child(SupPid,
			       {boss_websocket_router, {boss_websocket_router, start_link, []},
				permanent, 5000, worker, [boss_websocket_router]}),
    {ok, BossWebSocketRouterPid};


start_services(SupPid, Services) ->
    lists:foldl(
      fun([], Acc) -> Acc ;
	 ({ServiceUrl, Service}, Acc) ->
	      {ok, ServicePid} = 
		  supervisor:start_child(SupPid,
					 {Service, {boss_service_worker, start_link, [Service, ServiceUrl]},
					  permanent, 5000, worker, [Service]}),
	      Acc ++ [{ok, ServicePid}]
      end, 
      [], 
      Services ),
    {ok, SupPid}.

init([]) ->
    {ok, {{one_for_all, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
