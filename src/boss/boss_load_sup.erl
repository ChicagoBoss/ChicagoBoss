-module(boss_load_sup).
-author('chan.sisowath@gmail.com').

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() ->
    start_link([]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init(StartArgs) ->
    {ok, {{one_for_one, 10, 10}, [
                {load_controller, {boss_load_controller, start_link, [StartArgs]},
                    permanent,
                    2000,
                    worker,
                    [boss_load_controller]}
                ]}}.
