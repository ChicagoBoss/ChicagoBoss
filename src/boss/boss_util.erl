-module(boss_util).
-export([ensure_started/1]).

-spec ensure_started(atom()) -> 'ok'.
-spec ensure_stopped(atom()) -> 'ok'.

%% @spec ensure_started(atom()) -> ok
%% @doc start application.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec ensure_stopped(atom()) -> ok
%% @doc stop application.
ensure_stopped(App) ->
    case application:stop(App) of
        ok ->
            ok;
        {error, _} ->
            ok
    end.