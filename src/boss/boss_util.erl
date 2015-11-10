%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc
%%-------------------------------------------------------------------

-module(boss_util).
-export([ensure_started/1, ensure_stopped/1]).

-spec ensure_started(atom()) -> 'ok'.
-spec ensure_stopped(atom()) -> 'ok'.

%% @spec ensure_started(atom()) -> ok
%% @doc start application.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {"no such file or directory", _}} ->
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
