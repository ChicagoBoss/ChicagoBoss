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

-module(boss).
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the boss server.
start() ->
    is_compatible(erlang:system_info(otp_release)),
    boss_util:ensure_started(crypto),
    boss_util:ensure_started(mimetypes),
    application:start(boss).

%% @spec stop() -> ok
%% @doc Stop the boss server.
stop() ->
    Res = application:stop(boss),
    boss_util:ensure_stopped(mimetypes),
    boss_util:ensure_stopped(crypto),
    Res.

is_compatible("R16B03") ->
    _ = lager:emergency("Chicago Boss is not compatible with R16B03"),
    erlang:halt(1, "Chicago Boss is not compatible with R16B03");
is_compatible(_) ->
    ok.
