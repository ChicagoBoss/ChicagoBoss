-module(boss_util).
-export([ensure_started/1]).

-spec ensure_started(atom()) -> 'ok'.

%% @spec ensure_started(atom()) -> ok
%% @doc start application.
ensure_started(App) ->
    case application:start(App) of
    ok ->
        ok;
    {error, {already_started, App}} ->
        ok
    end.