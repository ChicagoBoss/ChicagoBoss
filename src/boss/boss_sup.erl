%% @author Evan Miller <emmiller@gmail.com>
%% @copyright YYYY author.

%% @doc Supervisor for the boss application.

-module(boss_sup).
-author('Evan Miller <emmiller@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = case application:get_env(port) of
        {ok, {env, PortVar}} ->
            case os:getenv(PortVar) of
                false -> 8001;
                PortStr -> list_to_integer(PortStr)
            end;
        {ok, P} when is_integer(P) -> P;
        undefined -> 8001
    end,
    WebConfig = [ {ip, Ip}, {port, Port} ],
    Web = {boss_web_controller,
	   {boss_web_controller, start_link, [WebConfig]},
	   permanent, 5000, worker, dynamic},

    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.
