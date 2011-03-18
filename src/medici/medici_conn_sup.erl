%%%-------------------------------------------------------------------
%%% File    : medici_conn_sup.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  6 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("medici.hrl").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(StartArgs) ->
    supervisor:start_link(?MODULE, StartArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(MediciOpts) ->
    ClientCount = proplists:get_value(num_connections, MediciOpts, ?NUM_CLIENTS),
    case proplists:get_bool(native, MediciOpts) of
	false ->
	    ChildList = [{list_to_atom("medici_connection_"++integer_to_list(ChildNum)), 
			  {medici_conn, start_link, [MediciOpts]},
			  permanent,
			  2000,
			  worker,
			  [medici_conn]} || ChildNum <- lists:seq(1, ClientCount)];
	true ->
	    ChildList = [{list_to_atom("medici_connection_"++integer_to_list(ChildNum)), 
			  {medici_native_conn, start_link, [MediciOpts]},
			  permanent,
			  2000,
			  worker,
			  [medici_native_conn]} || ChildNum <- lists:seq(1, ClientCount)]
    end,
    {ok,{{one_for_one,ClientCount*2,5}, ChildList}}.

%%====================================================================
%% Internal functions
%%====================================================================
