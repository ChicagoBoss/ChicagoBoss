-module(bmq_channel_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1, start_child/2, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_link(StartArgs) ->
    supervisor:start_link(?MODULE, StartArgs).

start_child(Supervisor, StartArgs) ->
    supervisor:start_child(Supervisor,
        {mq_channel_controller, {bmq_channel_controller, start_link, [StartArgs]},
            permanent,
            2000,
            worker,
            [bmq_channel_controller]}).

init(_StartArgs) ->
    {ok, {{one_for_one, 10, 10}, []}}.
