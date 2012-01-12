-module(boss_session_sup).
-author('Jose Luis Gordo Romero jose.gordo@tractis.com bassed on Evan Miller boss_db_sup.erl').

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() ->
    start_link([]).

start_link(StartArgs) ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, StartArgs).

init(StartArgs) ->
    {ok, {{one_for_one, 10, 10}, [
                {session_controller, {boss_session_controller, start_link, [StartArgs]},
                    permanent,
                    2000,
                    worker,
                    [boss_session_controller]}
                ]}}.
