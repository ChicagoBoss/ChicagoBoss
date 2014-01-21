-module(boss_session_sup).
-author('Jose Luis Gordo Romero jose.gordo@tractis.com bassed on Evan Miller boss_db_sup.erl').

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() ->
    start_link([]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init(StartArgs) ->
    Args = [{name, {local, boss_session_pool}},
            {worker_module, boss_session_controller},
            {size, 20}, 
            {max_overflow, 40}
            | StartArgs],
    PoolSpec  = {session_controller, 
                 {poolboy, start_link, [Args]},
                 permanent, 2000, worker, [poolboy]},
    {ok, {{one_for_one, 10, 10}, [PoolSpec]}}.
