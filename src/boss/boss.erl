%% @author Evan Miller <emmiller@gmail.com>
%% @copyright 2009 author.

%% @doc TEMPLATE.

-module(boss).
-author('Evan Miller <emmiller@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the boss server.
start() ->
    ensure_started(crypto),
    Res = application:start(boss),
    case application:get_env(template_compiler) of
        {ok, erlydtl} -> ok;
        _ -> ensure_started(etcher)
    end,
    Res.

%% @spec stop() -> ok
%% @doc Stop the boss server.
stop() ->
    Res = application:stop(boss),
    application:stop(crypto),
    Res.
