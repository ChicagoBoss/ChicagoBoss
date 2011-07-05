-module(boss_mq_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() ->
    start_link([]).

start_link(StartArgs) ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, StartArgs).

init(StartArgs) ->
    {ok, {{one_for_one, 10, 10}, [
                {mq_controller, {boss_mq_controller, start_link, [StartArgs]},
                    permanent,
                    2000,
                    worker,
                    [boss_mq_controller]}
                ]}}.

