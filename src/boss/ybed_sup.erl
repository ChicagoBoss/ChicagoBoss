-module(ybed_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    YBed = {ybed, {ybed,start,[Port]},
            permanent,2000,worker,[ybed]},
    {ok,{{one_for_all,0,1}, [YBed]}}.
