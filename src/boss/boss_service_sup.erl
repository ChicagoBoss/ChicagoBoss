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
-export([start_link/0]).

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
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->    
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Service = {boss_service, {boss_service, start_link, []},
	      Restart, Shutdown, Type, [boss_service]},

    {ok, {SupFlags, [Service]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
