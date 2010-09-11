%% @author Evan Miller <emmiller@gmail.com>
%% @copyright YYYY author.

%% @doc Callbacks for the rr application.

-module(boss_app).
-author('Evan Miller <emmiller@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for boss.
start(_Type, _StartArgs) ->
    boss_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for boss.
stop(_State) ->
    ok.
