-module(bmq_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_link(StartArgs) ->
    supervisor:start_link(?MODULE, StartArgs).

init(StartArgs) ->
    {ok, {{one_for_one, 10, 10}, [
                {mq_controller, {bmq_controller, start_link, [StartArgs]},
                    permanent,
                    2000,
                    worker,
                    [bmq_controller]}
            ]}}.
